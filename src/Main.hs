import qualified Codec.Binary.Base64.String as Base64
import Control.Monad.Loops
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.List
import Control.Monad
import Foreign.Marshal.Utils (fromBool)
import System.IO

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
        draw coltab = return (DrawConfig (drawColors coltab) drawImage Nothing)
        handle :: Maybe Event -> Curses Bool
        handle Nothing = return False
        handle (Just ev) =
          return (ev == EventCharacter 'q' || ev == EventCharacter 'Q')
        -- drawImage = return ()
        drawImage = do putStr (goto 2 40 ++ "pbm: " ++ image2iTerm2 pbmImage)
                       putStr (goto 3 40 ++ "pgm: " ++ image2iTerm2 pgmImage)
                       putStr (goto 4 40 ++ "ppm: " ++ image2iTerm2 ppmImage)
                       putStr (goto 5 40 ++ "sixel: " ++ sixelImage)
                       hFlush stdout



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



data BlockCanvas = BlockCanvas (UArray (Int, Int) Bool)
  deriving (Eq, Ord, Show)

drawBlockCanvas :: Integer -> Integer -> BlockCanvas -> Drawable
drawBlockCanvas j0 i0  (BlockCanvas arr) =
  drawAll
  where ((imin, jmin), (imax, jmax)) = bounds arr
        inbounds i j = imin <= i && i <= imax && jmin <= j && j <= jmax
        getBit i j = fromBool (if inbounds i j then arr ! (i, j) else False)
        drawChar i j = blocks ! ((getBit  i     j    `shift` 0) .|.
                                 (getBit  i    (j+1) `shift` 1) .|.
                                 (getBit (i+1)  j    `shift` 2) .|.
                                 (getBit (i+1) (j+1) `shift` 3))
        drawRow i = DrawAt (i0 + fromIntegral (i `div` 2)) j0
                    (DrawString [drawChar i j | j <- [jmin, jmin+2 .. jmax]])
        drawAll = DrawSome [drawRow i | i <- [imin, imin+2 .. imax]]

blockCanvas :: BlockCanvas
blockCanvas =
  sketch2canvas sketch
  where sketch2canvas = BlockCanvas .
                        listArray ((0, 0), (rows-1, cols-1)) .
                        map (not . isspace) .
                        join
        isspace c = c == ' ' || c == '.'
        rows = length sketch
        cols = length (head sketch)
        sketch = [ ".****..****...**.....****...**..**..*****."
                 , "**....**..**..**....**..**..**..**..**..**"
                 , "**....**..**..**....**..**..**..**..*****."
                 , "**....**..**..**....**..**..**..**..**..**"
                 , ".****..****...*****..****....****...**..**"]



-- 0b3210   0b [LR] [LL] [UR] [UL]
--
--   01
--   23
blocks :: UArray Word Char
blocks = array (0b0000, 0b1111) [ (0b0000, ' ')
                                , (0b0001, '▘')
                                , (0b0010, '▝')
                                , (0b0011, '▀')

                                , (0b0100, '▖')
                                , (0b0101, '▌')
                                , (0b0110, '▞')
                                , (0b0111, '▛')

                                , (0b1000, '▗')
                                , (0b1001, '▚')
                                , (0b1010, '▐')
                                , (0b1011, '▜')

                                , (0b1100, '▄')
                                , (0b1101, '▙')
                                , (0b1110, '▟')
                                , (0b1111, '█')
                                ]



goto :: Int -> Int -> String
goto r c = "\ESC[" ++ show r ++ ";" ++ show c ++ "H"




detectITerm2 :: Handle -> Handle -> IO Bool
detectITerm2 hi ho =
  do oldbuffering <- hGetBuffering stdin
     hSetBuffering stdin NoBuffering
     oldecho <- hGetEcho stdin
     hSetEcho stdin False
     hPutStr ho "\ESC]1337;ReportCellSize\BEL"
     hFlush ho
     rdy <- hWaitForInput hi 10
     -- if rdy
     --   then do reply <- whileM (hReady hi) (hGetChar hi)
     --           putStrLn ("got input: [" ++ show reply ++ "]")
     --   else putStrLn "no input"
     _ <- if rdy
          then whileM (hReady hi) (hGetChar hi)
          else return []
     hSetBuffering stdin oldbuffering
     hSetEcho stdin oldecho
     return rdy

-- detectXterm :: Handle -> Handle -> IO Bool
-- likely: printf "\e[c"         # Send Device Attributes (Primary DA)
-- likely: printf "\e[>c"        # Send Device Attributes (Secondary DA)
-- maybe:  printf "\e[5n"        # Device Status Report (DSR)
-- maybe:  printf "\e[20t"       # Window manipulation
-- maybe:  printf "\e[21t"       # Window manipulation
-- maybe:  printf "\e]4;0;?\a"   # Set Text Parameters (Change Color Number 0)
-- likely: printf "\e]15;?\a"    # Set Text Parameters (Tektronix foreground color)
-- likely: printf "\e]50;?\a"    # Set Text Parameters (Set Font)


-- See <https://www.iterm2.com/documentation-images.html>
image2iTerm2 :: String -> String
image2iTerm2 img =
  "\ESC]1337;File=height=1;inline=1:" ++ Base64.encode img ++ "\BEL"

pbmImage :: String
pbmImage = unlines
           [ "P1"
           , "16 16"
           , "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1"
           , "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
           ]

pgmImage :: String
pgmImage = unlines
           [ "P2"
           , "16 16"
           , "4"
           , "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
           , "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0"
           , "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0"
           , "0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0"
           , "0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0"
           , "0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0"
           , "0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0"
           , "0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0"
           , "0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0"
           , "0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0"
           , "0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0"
           , "0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0"
           , "0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0"
           , "0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0"
           , "0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0"
           , "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
           ]

ppmImage :: String
ppmImage = unlines $
           [ "P3"
           , "16 16"
           , "1"
           ] ++
           map (intercalate "  " . map translate)
           [ "bbbbbbbbbbbbbbbb" :: String
           , "brrrrrrrpppppppb"
           , "brrrrrrrpppppppb"
           , "brrrrrrrpppppppb"
           , "brrrrrrrpppppppb"
           , "brrrrrrrpppppppb"
           , "bgggggggcccccccb"
           , "bgggggggcccccccb"
           , "bgggggggcccccccb"
           , "bgggggggcccccccb"
           , "blllllllyyyyyyyb"
           , "blllllllyyyyyyyb"
           , "blllllllyyyyyyyb"
           , "blllllllyyyyyyyb"
           , "blllllllyyyyyyyb"
           , "bbbbbbbbbbbbbbbb"
           ]
  where translate :: Char -> String
        translate c | c == 'b' = "0 0 0" -- black
                    | c == 'w' = "1 1 1" -- white
                    | c == 'r' = "1 0 0" -- red
                    | c == 'g' = "0 1 0" -- green
                    | c == 'l' = "0 0 1" -- blue
                    | c == 'p' = "0 1 1" -- pink
                    | c == 'c' = "1 0 1" -- cyan
                    | c == 'y' = "1 1 0" -- yellow
                    | otherwise = ""

sixelImage :: String
sixelImage =
  "\ESCP0;0;8;q" ++
  "\"1;1" ++
  "#0;2;0;0;0" ++
  "#1;2;100;100;100" ++
  "#2;2;100;0;0" ++
  "#0" ++
  sx 0b111111 ++ "!10" ++ sx 0b000001 ++ sx 0b111111 ++ "$" ++
  "#2" ++
  sx 0b000000 ++ sx 0b111110 ++ "!8" ++ sx 0b000010 ++ sx 0b111110 ++ sx 0b000000 ++ "$" ++
  "-" ++
  "#0" ++
  sx 0b111111 ++ "!10" ++ sx 0b100000 ++ sx 0b111111 ++ "$" ++
  "#2" ++
  sx 0b000000 ++ sx 0b011111 ++ "!8" ++ sx 0b010000 ++ sx 0b011111 ++ sx 0b000000 ++ "$" ++
  "-" ++
  "\ESC\\"
  where sx x = [chr (x + 0x3f)]



drawColors :: ColorTable -> Drawable
drawColors coltab =
  DrawWithSize $ \ny nx ->
  DrawSome ([ DrawBorder (BorderGlyphs
                           (Just glyphLineV)     -- Glyph '│' []
                           (Just glyphLineV)     -- Glyph '│' []
                           (Just glyphLineH)     -- Glyph '─' []
                           (Just glyphLineH)     -- Glyph '─' []
                           (Just (Glyph '╭' [])) -- glyphCornerUL
                           (Just (Glyph '╮' [])) -- glyphCornerUR
                           (Just (Glyph '╰' [])) -- glyphCornerLL
                           (Just (Glyph '╯' [])) -- glyphCornerLR
                         )
            , DrawAt 0 ((nx - 14) `div` 2) (DrawString " Colour Table ")
            , DrawAt 1 (nx - 12) (DrawString ("lines:  " ++ show ny))
            , DrawAt 2 (nx - 12) (DrawString ("column: " ++ show nx))
            -- , DrawAt 1 (nx `div` 2) (DrawString ("[" ++ image2iTerm2 pbmImage ++ "]"))
            -- , DrawAt 2 (nx `div` 2) (DrawString (image2iTerm2 pgmImage))
            -- , DrawAt 3 (nx `div` 2) (DrawString (image2iTerm2 ppmImage))
            , drawBlockCanvas 1 1 blockCanvas
            , DrawAt (ny - 1) (nx - 19 - 1) (DrawString " (press Q to quit) ")
            ] ++
            (zipWith drawColor [4..] coltab))
  where drawColor :: Integer -> (String, ColorID) -> Drawable
        drawColor ln (name, cid) =
          DrawSome
          [ DrawAt ln 10 (DrawString name)
          , DrawAt ln 25 (DrawColored cid (DrawString name))
          ]
