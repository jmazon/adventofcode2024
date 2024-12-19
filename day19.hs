import Control.Monad (forM_)
import Control.Monad.ST (ST,runST)
import Data.Array.ST (STArray,newArray,readArray,writeArray)
import Data.List (tails)
import Data.Maybe (mapMaybe)

parse :: String -> ([String],[String])
parse s = (patterns,towels) where
  (rawPatterns:_:towels) = lines s
  patterns = words (filter (/= ',') rawPatterns)

main :: IO ()
main = interact $ show . part2 . parse

match :: String -> String -> Maybe Int
match = go 0 where
  go i (s:string) (p:prefix)
    | s == p = go (i+1) string prefix
    | otherwise = Nothing
  go i _ [] = Just i
  go _ _ _ = Nothing

part2 :: ([String],[String]) -> Int
part2 (patterns,towels) = sum $ map dp towels where
  dp towel = runST $ do
    let l = length towel
    a <- newArray (0,l) 0 :: ST s (STArray s Int Int)
    writeArray a 0 1
    forM_ (zip [0..] (tails towel)) $ \(i,suffix) -> do
      cur <- readArray a i
      forM_ (mapMaybe (match suffix) patterns) $ \l' -> do
        let i' = i + l'
        prev <- readArray a i'
        writeArray a i' (prev + cur)
    readArray a l
