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

import Control.Monad.State
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
                           , stateWindows :: [Window]
                           , statePanels :: [Panel]
                           }

type DrawStateM = StateT DrawState Curses

withWindow' :: DrawState -> Update a -> Curses a
withWindow' s = updateWindow (stateWindow s)

withWindow :: Update a -> DrawStateM a
withWindow f =
  do s <- get
     lift (withWindow' s f)

draw :: Drawable -> DrawStateM ()
draw (DrawString str) = withWindow (drawString str)
draw (DrawText text) = withWindow (drawText text)
draw (DrawGlyph glyph) = withWindow (drawGlyph glyph)
draw (DrawBorder (BorderGlyphs g1 g2 g3 g4 g5 g6 g7 g8)) =
  withWindow (drawBorder g1 g2 g3 g4 g5 g6 g7 g8)
draw (DrawBox (BoxGlyphs g1 g2)) = withWindow (drawBox g1 g2)
draw (DrawLineH g y) = withWindow (drawLineH g y)
draw (DrawLineV g x) = withWindow (drawLineV g x)
draw (DrawAt y x d) =
  do w <- stateWindow <$> get
     (y0, x0) <- lift (getCursor w)
     withWindow (moveCursor y x)
     draw d
     withWindow (moveCursor y0 x0)
draw (DrawWithAttribute a b d) =
  do as0 <- stateAttributes <$> get
     withWindow (setAttribute a b)
     modify $ \s -> let as = if b
                          then union [a] as0
                          else delete a as0
                    in s { stateAttributes = as }
     draw d
     withWindow (setAttributes as0)
     modify $ \s -> s { stateAttributes = as0 }
draw (DrawWithAttributes as d) =
  do as0 <- stateAttributes <$> get
     withWindow (setAttributes as)
     modify $ \s -> s { stateAttributes = as }
     draw d
     withWindow (setAttributes as0)
     modify $ \s -> s { stateAttributes = as0 }
draw (DrawColored c d) =
  do c0 <- stateColorID <$> get
     withWindow (setColor c)
     modify $ \s -> s { stateColorID = c }
     draw d
     withWindow (setColor c0)
     modify $ \s -> s { stateColorID = c0 }
draw (DrawWindow (WindowShape ny nx y x) d) =
  do w0 <- stateWindow <$> get
     w <- lift (newWindow ny nx y x)
     modify $ \s -> s { stateWindows = w : stateWindows s }
     modify $ \s -> s { stateWindow = w }
     draw d
     modify $ \s -> s { stateWindow = w0 }
draw (DrawPanels ps) =
  mapM_ drawPanel (reverse ps)
  where drawPanel :: (PanelShape, Drawable) -> DrawStateM ()
        drawPanel (PanelShape ny nx y x, d) =
          do w0 <- stateWindow <$> get
             w <- lift (newWindow ny nx y x)
             modify $ \s -> s { stateWindows = w : stateWindows s }
             p <- lift (newPanel w)
             modify $ \s -> s { statePanels = p : statePanels s }
             modify $ \s -> s { stateWindow = w }
             draw d
             modify $ \s -> s { stateWindow = w0 }
draw (DrawSome ds) = mapM_ draw ds



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
             DrawState _ _ _ ws ps <-
               execStateT (draw d) (DrawState w c [] [] [])
             refreshPanels
             render
             mapM_ deletePanel ps
             mapM_ closeWindow ws
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
