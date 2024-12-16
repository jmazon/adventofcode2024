import Control.Arrow ((&&&))
import Control.Lens
import Data.Char
import Data.List.Split
import Data.Ratio
import Data.Maybe
import Linear.Matrix
import Linear.V2
import Linear.Vector

type V = V2 Int

parse :: String -> [(V,V,V)]
parse = map triplet . chunksOf 4 . map (toV . map read . words . filter ((||) <$> isDigit <*> (== ' '))) . lines
  where
    triplet (a:b:c:_) = (a,b,c)
    toV [x,y] = V2 x y

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

part1 :: [(V,V,V)] -> Int
part1 = sum . mapMaybe solve  where
  solve :: (V,V,V) -> Maybe Int
  solve (a,b,g) = case go (min 100 ((g ^. aAxis) `div` (a ^. aAxis))) of
      [] -> Nothing
      xs -> Just (minimum xs)
    where
      aAxis | a ^. _x > 0 = _x
            | a ^. _y > 0 = _y
      bAxis | b ^. _x > 0 = _x
            | b ^. _y > 0 = _y
      go :: Int -> [Int]
      go da | da < 0 = []
            | otherwise =
              let db = ((g - da*^a) ^. bAxis) `div` (b ^. bAxis)
              in if | db > 100 -> []
                    | db < 0 -> go (da-1)
                    | da*^a + db*^b == g -> (3*da+db) : go (da-1)
                    | otherwise -> go (da-1)

part2 :: [(V,V,V)] -> Int
part2 = sum . mapMaybe solve where
  solve :: (V,V,V) -> Maybe Int
  solve (a,b,g0)
      | det22 m == 0 = error "det 0"
      | all ((== 1) . denominator) i = Just $ sum (liftU2 (*) (numerator <$> i) (V2 3 1))
      | otherwise = Nothing
    where
      g = fromIntegral <$> g0 + V2 10000000000000 10000000000000 :: V2 (Ratio Int)
      m = fmap fromIntegral <$> V2 a b
      i = luSolveFinite (transpose m) g
