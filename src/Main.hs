import UI.NCurses
import UI.NCurses.HL



type State = ColorTable

main :: IO ()
main = runCursesHL setup update draw handle
  where setup :: Curses (Maybe State)
        setup = do setEcho False
                   coltab <- makeColorTable
                   return (Just coltab)
        update :: Bool -> State -> Curses (Maybe State)
        update quit coltab =
          do if quit
               then return Nothing
               else return (Just coltab)
        draw :: State -> Curses DrawConfig
        draw coltab =
          do return (DrawConfig
                      (DrawSome
                        [ DrawAt 1 10 (DrawString "Hello, World!")
                        , DrawAt 3 10 (DrawString "(press q to quit)")
                        , drawColors coltab
                        ])
                      Nothing)
        handle :: Maybe Event -> Curses Bool
        handle Nothing = return False
        handle (Just ev) =
          return (ev == EventCharacter 'q' || ev == EventCharacter 'Q')



type ColorTable = [(String, ColorID)]

makeColorTable :: Curses ColorTable
makeColorTable =
  seq' cols
  where seq' :: Monad m => [(a, m b)] -> m [(a, b)]
        seq' xs = let (ns, cs) = unzip xs
                  in zip ns <$> sequence cs
        cols :: [(String, Curses ColorID)]
        cols = [ ("black"         , newColorID ColorBlack   ColorDefault  1)
               , ("red"           , newColorID ColorRed     ColorDefault  2)
               , ("green"         , newColorID ColorGreen   ColorDefault  3)
               , ("yellow"        , newColorID ColorYellow  ColorDefault  4)
               , ("blue"          , newColorID ColorBlue    ColorDefault  5)
               , ("magenta"       , newColorID ColorMagenta ColorDefault  6)
               , ("cyan"          , newColorID ColorCyan    ColorDefault  7)
               , ("white"         , newColorID ColorWhite   ColorDefault  8)
               , ("bright black"  , newColorID (Color  8)   ColorDefault  9)
               , ("bright red"    , newColorID (Color  9)   ColorDefault 10)
               , ("bright green"  , newColorID (Color 10)   ColorDefault 11)
               , ("bright yellow" , newColorID (Color 11)   ColorDefault 12)
               , ("bright blue"   , newColorID (Color 12)   ColorDefault 13)
               , ("bright magenta", newColorID (Color 13)   ColorDefault 14)
               , ("bright cyan"   , newColorID (Color 14)   ColorDefault 15)
               , ("bright white"  , newColorID (Color 15)   ColorDefault 16)
               ]



drawColors :: ColorTable -> Drawable
drawColors coltab =
  DrawSome (drawColor <$> zip [5..] coltab)
  where drawColor :: (Integer, (String, ColorID)) -> Drawable
        drawColor (ln, (name, cid)) =
          DrawSome
          [ DrawAt ln 10 (DrawString name)
          , DrawAt ln 25 (DrawColored cid (DrawString name))
          ]
