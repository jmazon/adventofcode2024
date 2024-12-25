import Control.Arrow ((***))
import Control.Monad (guard)
import Data.List (nub,partition,transpose)
import Data.List.Split (wordsBy)

newtype Lock = Lock [Int] deriving Show
newtype Key = Key [Int] deriving Show

parse :: String -> ([Lock],[Key])
parse s = (map readLock *** map readKey) (partition isLock shapes) where
  ls = lines s
  shapes = wordsBy (== "") ls
  isLock = ((== "#") . nub . head)
  readLock = Lock . map (pred . length . takeWhile (== '#')) . transpose
  readKey = Key . map (pred . length . dropWhile (== '.')) . transpose

fit :: Lock -> Key -> Bool
fit (Lock ls) (Key ks) = all (<= 5) (zipWith (+) ls ks)

solve :: ([Lock],[Key]) -> Int
solve (ls,ks) = length $ (guard @[] .) . fit <$> ls <*> ks

main :: IO ()
main = interact $ show . solve . parse
