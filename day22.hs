import Control.Arrow ((&&&))
import Data.Bits (xor)
import Data.List (tails)
import qualified Data.Map.Strict as Map

nextSecret :: Int -> Int
nextSecret =
  prune . (mix <*> (2048 *)) .
  prune . (mix <*> (`div` 32)) .
  prune . (mix <*> (64 *))

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate nextSecret)

prefixPrices :: Int -> [([Int],Int)]
prefixPrices secret =
  let prices = (`mod` 10) <$> iterate nextSecret secret
      changes = take 2000 $ zipWith (-) (tail prices) prices
      prefixes = filter ((== 4) . length) $ take 4 <$> tails changes
  in zip prefixes (drop 4 prices)

ppMap :: Int -> Map.Map [Int] Int
ppMap = Map.fromListWith (flip const) . prefixPrices

part2 :: [Int] -> Int
part2 = maximum . Map.unionsWith (+) . map ppMap where
