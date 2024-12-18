import Control.Arrow ((&&&))
import Data.Ix (inRange)
import qualified Data.Set as Set

parse :: String -> [(Int,Int)]
parse = map f . lines where
  f l = (read x,read y) where (x,_:y) = break (== ',') l

main :: IO ()
main = interact $ show . (part1 1024 &&& part2) . parse

goalCoord :: Int
goalCoord = 70

part1 :: Int -> [(Int,Int)] -> Maybe Int
part1 threshold ps = bfs Set.empty [((0,0),0)] where
  corrupted = Set.fromList (take threshold ps)
  valid p = inRange ((0,0),(goalCoord,goalCoord)) p && p `Set.notMember` corrupted
  bfs _ [] = Nothing
  bfs cl ((p@(x,y),d):q)
    | p == (goalCoord,goalCoord) = Just d
    | p `Set.member` cl = bfs cl q
    | otherwise = bfs cl' (q ++ q') where
        cl' = Set.insert p cl
        q' = map (,d+1) $ filter (`Set.notMember` cl) $
             filter valid [(x-1,y),(x,y+1),(x+1,y),(x,y-1)]

part2 :: [(Int,Int)] -> (Int,Int)
part2 ps = bsearch 0 (length ps + 1) where
  bsearch a b
    | b <= a + 1 = ps !! a
    | Just _ <- part1 m ps = bsearch m b
    | otherwise = bsearch a m
    where m = (a + b) `div` 2
