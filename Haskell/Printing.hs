module Printing where

-- print strings from list in separate lines
printLinesWithoutSplit :: [String] -> IO ()
printLinesWithoutSplit xs = putStr (unlines xs)

printLines :: [String] -> IO ()
printLines xs = putStr (unlines (convertLongStrings xs))

splitString :: String -> [String]
splitString "" = [""]
splitString str = go [] [] (words str)
  where
    go :: [String] -> String -> [String] -> [String]
    go acc [] [] = reverse acc
    go acc curr [] = reverse (curr : acc)
    go acc curr (w:ws)
      | length (unwords [curr, w]) <= 50 = go acc (if null curr then w else unwords [curr, w]) ws
      | null curr = go (w : acc) [] ws
      | otherwise = go (curr : acc) [] (w:ws)

convertLongStrings :: [String] -> [String]
convertLongStrings = concatMap splitString
