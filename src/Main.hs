import UI.NCurses
import UI.NCurses.HL



type State = ColorTable

main :: IO ()
main = runCursesHL setup update draw handle
  where setup :: Curses (Maybe State)
        setup = do setCursorMode CursorInvisible
                   setEcho False
                   coltab <- makeColorTable
                   return (Just coltab)
        update :: Bool -> State -> Curses (Maybe State)
        update quit coltab =
          do if quit
               then return Nothing
               else return (Just coltab)
        draw :: State -> Curses DrawConfig
        draw coltab = return (DrawConfig (drawColors coltab) Nothing)
        handle :: Maybe Event -> Curses Bool
        handle Nothing = return False
        handle (Just ev) =
          return (ev == EventCharacter 'q' || ev == EventCharacter 'Q')



type ColorTable = [(String, ColorID)]

makeColorTable :: Curses ColorTable
makeColorTable =
  seq' (zipWith newcol [1..] cols)
  where seq' :: Monad m => [(a, m b)] -> m [(a, b)]
        seq' xs = let (ns, cs) = unzip xs
                  in zip ns <$> sequence cs
        newcol :: Integer -> (String, Color) -> (String, Curses ColorID)
        newcol i (s, c) = (s, newColorID c ColorDefault i)
        cols :: [(String, Color)]
        cols = [ ("black"         , ColorBlack  )
               , ("red"           , ColorRed    )
               , ("green"         , ColorGreen  )
               , ("yellow"        , ColorYellow )
               , ("blue"          , ColorBlue   )
               , ("magenta"       , ColorMagenta)
               , ("cyan"          , ColorCyan   )
               , ("white"         , ColorWhite  )
               , ("bright black"  , Color  8    )
               , ("bright red"    , Color  9    )
               , ("bright green"  , Color 10    )
               , ("bright yellow" , Color 11    )
               , ("bright blue"   , Color 12    )
               , ("bright magenta", Color 13    )
               , ("bright cyan"   , Color 14    )
               , ("bright white"  , Color 15    )
               ]



drawColors :: ColorTable -> Drawable
drawColors coltab =
  DrawWithSize $ \ny nx ->
  DrawSome ([ DrawBorder (BorderGlyphs
                           (Just glyphLineV)
                           (Just glyphLineV)
                           (Just glyphLineH)
                           (Just glyphLineH)
                           (Just glyphCornerUL)
                           (Just glyphCornerUR)
                           (Just glyphCornerLL)
                           (Just glyphCornerLR)
                         )
            , DrawAt 0 ((nx - 14) `div` 2) (DrawString " Colour Table ")
            , DrawAt (ny - 1) (nx - 19 - 1) (DrawString " (press Q to quit) ")
            ] ++
            (zipWith drawColor [4..] coltab))
  where drawColor :: Integer -> (String, ColorID) -> Drawable
        drawColor ln (name, cid) =
          DrawSome
          [ DrawAt ln 10 (DrawString name)
          , DrawAt ln 25 (DrawColored cid (DrawString name))
          ]
