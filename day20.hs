import Control.Arrow ((&&&))
import Data.Array (Array,listArray,bounds,(!),indices,inRange,range)
import Data.Array.ST (runSTArray,newArray,readArray,writeArray)
import Data.List (find)
import Linear.V2 (V2(V2))

type V = V2 Int

manhattan :: V -> Int
manhattan = sum . abs

dirs :: [V]
dirs = [V2 (-1) 0,V2 0 1,V2 1 0,V2 0 (-1)]

parse :: String -> Array V Char
parse s = listArray (V2 1 1,V2 h w) (concat raw) where
  raw = lines s
  h = length raw
  w = length (head raw)

jumps1 :: [V]
jumps1 = map (2 *) dirs

solve :: [V] -> Array V Char -> Int
solve jumps g = length (filter (>= 100) cheats) where
  Just goalPos = find ((== 'E') . (g!)) (indices g)
  bfs [] a = pure a
  bfs ((p,d):q) a = do
    prev <- readArray a p
    enqueue <- if prev < maxBound
               then pure id
               else do
                 writeArray a p d
                 pure $ flip (++)
                      $ map (,d+1)
                      $ filter ((/= '#') . (g!))
                      $ filter (inRange (bounds g))
                      $ map (p +) dirs
    bfs (enqueue q) a
  dists = runSTArray (newArray (bounds g) maxBound >>= bfs [(goalPos,0)])
  eval p d
    | g!p == '#' = minBound
    | p' <- p+d =
        if | not (inRange (bounds g) p') -> minBound
           | g!p' == '#' -> minBound
           | otherwise -> dists!p - dists!p' - manhattan (p'-p)
  cheats = eval <$> indices g <*> jumps

jumps2 :: [V]
jumps2 = let r = 20 in
  filter ((<= r) . manhattan) (range (V2 (-r) (-r),V2 r r))
  
main :: IO ()
main = interact $ show . (solve jumps1 &&& solve jumps2) . parse
