module UI.NCurses.HL
  ( Drawable(..)
  , BorderGlyphs(..)
  , BoxGlyphs(..)
  , WindowShape(..)
  , PanelShape(..)
  , DrawState(..)
  , DrawConfig(..)
  , runCursesHL
  ) where

import Data.List
import qualified Data.Text as T

import UI.NCurses
import UI.NCurses.Panel



-- | A 'Drawable' is something that can be drawn by NCurses
data Drawable = DrawString String
              | DrawText T.Text
              | DrawGlyph Glyph
              | DrawBorder BorderGlyphs
              | DrawBox BoxGlyphs
              | DrawLineH (Maybe Glyph) Integer
              | DrawLineV (Maybe Glyph) Integer
              | DrawAt Integer Integer Drawable
              | DrawWithAttribute Attribute Bool Drawable
              | DrawWithAttributes [Attribute] Drawable
              | DrawColored ColorID Drawable
              | DrawWindow WindowShape Drawable
              | DrawPanels [(PanelShape, Drawable)]
              | DrawSome [Drawable]
  deriving (Eq, Show)

-- | Border definition
data BorderGlyphs = BorderGlyphs
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
                    (Maybe Glyph)
  deriving (Eq, Show)

-- | Box definition
data BoxGlyphs = BoxGlyphs
                 (Maybe Glyph)
                 (Maybe Glyph)
  deriving (Eq, Show)

-- | Window shape (layout) definition
data WindowShape = WindowShape Integer Integer Integer Integer
  deriving (Eq, Ord, Read, Show)

-- | Panel shape (layout) definition
data PanelShape = PanelShape Integer Integer Integer Integer
  deriving (Eq, Ord, Read, Show)



-- | Our approximation of the internal state of NCurses while drawing
data DrawState = DrawState { stateWindow :: Window
                           , stateColorID :: ColorID
                           , stateAttributes :: [Attribute]
                           }

withWindow :: DrawState -> Update a -> Curses a
withWindow (DrawState w _ _) = updateWindow w

draw :: DrawState -> Drawable -> Curses ()
draw s (DrawString str) = withWindow s (drawString str)
draw s (DrawText text) = withWindow s (drawText text)
draw s (DrawGlyph glyph) = withWindow s (drawGlyph glyph)
draw s (DrawBorder (BorderGlyphs g1 g2 g3 g4 g5 g6 g7 g8)) =
  withWindow s (drawBorder g1 g2 g3 g4 g5 g6 g7 g8)
draw s (DrawBox (BoxGlyphs g1 g2)) = withWindow s (drawBox g1 g2)
draw s (DrawLineH g y) = withWindow s (drawLineH g y)
draw s (DrawLineV g x) = withWindow s (drawLineV g x)
draw s (DrawAt y x d) =
  do (y0, x0) <- getCursor (stateWindow s)
     withWindow s (moveCursor y x)
     draw s d
     withWindow s (moveCursor y0 x0)
draw s (DrawWithAttribute a b d) =
  do withWindow s (setAttribute a b)
     let as = stateAttributes s
         as' = if b
               then union [a] as
               else delete a as
     draw s { stateAttributes = as' } d
     withWindow s (setAttributes (stateAttributes s))
draw s (DrawWithAttributes as d) =
  do withWindow s (setAttributes as)
     draw s { stateAttributes = as } d
     withWindow s (setAttributes (stateAttributes s))
draw s (DrawColored c d) =
  do withWindow s (setColor c)
     draw s { stateColorID = c } d
     withWindow s (setColor (stateColorID s))
draw s (DrawWindow (WindowShape ny nx y x) d) =
  do w <- newWindow ny nx y x   -- TODO: close window
     draw s d
draw s (DrawPanels ps) =
  mapM_ drawPanel (reverse ps)
  where drawPanel :: (PanelShape, Drawable) -> Curses ()
        drawPanel (PanelShape ny nx y x, d) =
          do w <- newWindow ny nx y x -- TODO: close window
             p <- newPanel w          -- TODO : delete panel
             draw s { stateWindow = w } d
draw w (DrawSome ds) = mapM_ (draw w) ds



data DrawConfig = DrawConfig Drawable (Maybe Integer)
  deriving (Eq, Show)

runCursesHL :: forall s t.
               Curses (Maybe s)
            -> (t -> s -> Curses (Maybe s))
            -> (s -> Curses DrawConfig)
            -> (Maybe Event -> Curses t)
            -> IO ()
runCursesHL minit rhs app2curses curses2app =
  runCurses (iterateWhileJustM_ step minit)
  where step :: s -> Curses (Maybe s)
        step s =
          do DrawConfig d to <- app2curses s
             w <- defaultWindow
             let c = defaultColorID
             draw (DrawState w c []) d
             refreshPanels
             render
             ev <- getEvent w to
             t <- curses2app ev
             rhs t s



-- | Iterate while the state is 'Just a'
iterateWhileJustM_ :: forall m a. Monad m
                   => (a -> m (Maybe a)) -> m (Maybe a) -> m ()
iterateWhileJustM_ upd mst0 =
  loop =<< mst0
  where loop :: Maybe a -> m ()
        loop mst =
          do case mst of
               Nothing -> return ()
               Just st -> do mst' <- upd st
                             loop mst'
