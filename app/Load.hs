module Load where

import Types

-------------------------------------------------------------------------------

makeRow :: String -> Int -> Level
makeRow row y =
  [ ( ( (fromIntegral x * tileSize) - ((1024 / 2) - (tileSize / 2)),
        (fromIntegral y * tileSize) - ((768 / 2) - (tileSize / 2))
      ),
      row !! x
    )
    | x <- [0 .. length row - 1],
      row !! x == 's' || row !! x == 'b' || row !! x == '*' || row !! x == '@' || row !! x == '.'
  ]

prepareData :: [String] -> Level
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0 .. length rawData - 1]]