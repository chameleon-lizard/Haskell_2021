module Load where

import Types

-------------------------------------------------------------------------------

getCellTypeFromChar :: Char -> CellType 
getCellTypeFromChar c 
  | c == 'b' = Bx
  | c == 'f' = Fi
  | c == '.' = Fl
  | c == '@' = Pl
  | c == 's' = Ds
  | c == '*' = Wl
  | otherwise = Fl

makeRow :: String -> Int -> Level
makeRow row y =
  [ ( ( (fromIntegral x * tileSize) - ((1024 / 2) - (tileSize / 2)),
        (fromIntegral y * tileSize) - ((768 / 2) - (tileSize / 2))
      ),
      getCellTypeFromChar (row !! x)
    )
    | x <- [0 .. length row - 1]
  ]

prepareData :: [String] -> Level
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0 .. length rawData - 1]]