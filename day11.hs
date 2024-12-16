{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
import Control.Arrow ((&&&))
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

parse :: String -> [Int]
parse = map read . words

part1,part2 :: [Int] -> Int
part1 = length . (!!25) . iterate blink

blink :: [Int] -> [Int]
blink = concatMap change

change :: Int -> [Int]
change 0 = [1]
change n =
  let s = show n in
  if even (length s)
  then let (l,r) = splitAt (length s `div` 2) s in [read l,read r]
  else [n * 2024]

part2 ns = evalState (sum <$> mapM (blink2 75) ns) Map.empty

blink2 :: Int -> Int -> State (Map.Map (Int,Int) Int) Int
blink2 0 _ = pure 1
blink2 i n = do
  gets (Map.lookup (i,n)) >>= \case
    Just r -> pure r
    Nothing -> do
      r <- sum <$> mapM (blink2 (i-1)) (change n)
      modify (Map.insert (i,n) r)
      pure r
