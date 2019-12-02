module Main where

fuelRequired :: [Int] -> Int
fuelRequired xs = sum $ moduleFuel <$> xs
  where
    moduleFuel mass = floor (fromIntegral mass / 3.0) - 2

main :: IO ()
main = do
  contents <- readFile "part1-input.txt"
  let xs = (\mass -> read mass :: Int) <$> lines contents
  print $ fuelRequired xs
