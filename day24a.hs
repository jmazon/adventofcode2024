import Data.Bits (shiftL)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

parse :: String -> Map String Bool
parse s = m where
  m = Map.fromList $ map (parseLine . words) $ filter (not . null) $ lines s
  parseLine [var,val] = (init var,toEnum (read val))
  parseLine [v1,op,v2,_,dst] = (dst,parseOp op (m Map.! v1) (m Map.! v2))
  parseOp "AND" = (&&)
  parseOp  "OR" = (||)
  parseOp "XOR" = (/=)

part1 :: Map String Bool -> Int
part1 =
  foldl' (+) 0 . zipWith (flip shiftL) [0..] .
  map fromEnum . Map.elems . snd . Map.split "z0"

main :: IO ()
main = interact $ show . part1 . parse
