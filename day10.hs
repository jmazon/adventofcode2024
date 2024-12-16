import Debug.Trace

import Control.Arrow ((&&&))
import Data.Array
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe
import Linear.V2

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

traceMapId m = traceMap m m

traceMap :: Array (V2 Int) (Set.Set s) -> a -> a
traceMap m = trace s where
  (V2 a b,V2 c d) = bounds m
  s = unlines [ [ intToDigit (Set.size (m!V2 i j)) | j <- [b..d] ] | i <- [a..c] ]

part1 :: Array (V2 Int) Int -> Int
part1 m = sum $ fmap Set.size $ traceMapId $ foldl' f a0 [8,7..0]
  where
    a0 = array (bounds m) [ (i,if 9 == m!i then Set.singleton i else Set.empty) | i <- indices m ]
    f :: Array (V2 Int) (Set.Set (V2 Int)) -> Int -> Array (V2 Int) (Set.Set (V2 Int))
    f a h = traceMap a $
            accumArray Set.union Set.empty (bounds m)
            [ (i',s) | i <- indices a, s <- [a!i], d <- dirs, let i' = i+d, inRange (bounds m) i', m!i' == h ]

part2 :: Array (V2 Int) Int -> Int
part2 m = sum $ catMaybes $ elems $ foldl' f ((\i -> if i == 9 then Just 1 else Nothing) <$> m) [8,7..0]
  where
    f :: Array (V2 Int) (Maybe Int) -> Int -> Array (V2 Int) (Maybe Int)
    f a h = accum (\mb c -> fmap (+c) mb)
                  ((\i -> if i == h then Just 0 else Nothing) <$> m)
            [ (i',b) | i <- indices m, Just b <- [a!i], d <- dirs, let i' = i+d, inRange (bounds m) i' ]
