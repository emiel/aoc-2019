module Main where

moduleFuel :: Int -> Int
moduleFuel mass =
  let fuel = floor (fromIntegral mass / 3.0) - 2
   in if fuel > 0
        then fuel + moduleFuel fuel
        else 0

fuelRequired :: [Int] -> Int
fuelRequired xs = sum $ moduleFuel <$> xs

main :: IO ()
main = do
  contents <- readFile "part1-input.txt"
  let xs = (\mass -> read mass :: Int) <$> lines contents
  print $ fuelRequired xs
