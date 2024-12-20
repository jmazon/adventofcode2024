import Control.Arrow ((&&&))
import Data.Array (Ix,Array,listArray,bounds,(!),indices,accumArray,inRange,array,assocs)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Monoid (Sum(getSum))
import Linear.V2 (V2(V2))

import qualified Data.Set as Set

dirs :: [V2 Int]
dirs = [ V2 (-1) 0,V2 0 1,V2 1 0,V2 0 (-1) ]

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

parse :: String -> Array (V2 Int) Int
parse s = listArray (V2 1 1,V2 h w) (digitToInt <$> concat raw) where
  raw = lines s
  h = length raw
  w = length (head raw)

amap :: Ix e => ((e,a) -> b) -> Array e a -> Array e b
amap f a = array (bounds a) $ map (\ix@(i,_) -> (i,f ix)) $ assocs a

solve :: Monoid m => (V2 Int -> m) -> (m -> Int) -> Array (V2 Int) Int -> Int
solve fromIndex toInt m = sum $ fmap toInt $ foldl' f a0 [8,7..0]
  where
    a0 = amap (\(i,x) -> if x == 9 then fromIndex i else mempty) m
    f a h = accumArray (<>) mempty (bounds m)
            [ (i',a!i) | i <- indices m, d <- dirs, let i' = i+d, inRange (bounds m) i', m!i' == h ]

part1 :: Array (V2 Int) Int -> Int
part1 = solve Set.singleton Set.size

part2 :: Array (V2 Int) Int -> Int
part2 = solve (const 1) getSum
