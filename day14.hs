import Control.Arrow ((&&&))
import Data.Array
import Data.Char
import Data.List (group,sort)
import Data.Monoid

parse :: String -> [((Int,Int),(Int,Int))]
parse = map line . lines where
  line l = ((px,py),(vx,vy)) where
    (px,l') = readInt (drop 2 l)
    (py,l'') = readInt (drop 1 l')
    (vx,l''') = readInt (drop 3 l'')
    (vy,[]) = readInt (drop 1 l''')
  readInt s =
    let (n,s') = break (not . ((||) <$> isDigit <*> (== '-'))) s
    in (read n,s')

main :: IO ()
main = interact $ unlines . uncurry (:) . (show . part1 &&& part2) . parse

part1 :: [((Int,Int),(Int,Int))] -> Int
part1 = checkSum . map advance where
  advance ((px,py),(vx,vy)) = (dx,dy) where
    dx = (px+100*vx) `mod` 101
    dy = (py+100*vy) `mod` 103

checkSum :: [(Int,Int)] -> Int
checkSum = product' . foldMap quadrants where
  quadrants (dx,dy) =
    ( Sum (if dx < 50 && dy < 51 then 1 else 0)
    , Sum (if dx > 50 && dy < 51 then 1 else 0)
    , Sum (if dx < 50 && dy > 51 then 1 else 0)
    , Sum (if dx > 50 && dy > 51 then 1 else 0)
    )
  product' (Sum a,Sum b,Sum c,Sum d) = a*b*c*d

part2 :: [((Int,Int),(Int,Int))] -> [String]
part2 = concatMap p . filter noOverlap . zip [0..] . take (103*101) . iterate (map advance) where
  advance :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
  advance ((px,py),(vx,vy)) = (((px+vx) `mod` 101,(py+vy) `mod` 103),(vx,vy))
  p (i,rs) =
    let g = accumArray (+) 0 ((0,0),(100,102)) (map (fmap (const 2)) rs)
    in show i
       : [ [ if g!(x,y) > 0 then 'O' else '.'
           | x <- [0..100] ]
         | y <- [0..102] ]
  -- I haven't managed to decide if I considered this heuristic shameful or not:
  noOverlap (_,rs) =
    let nubbed = group $ sort $ map fst rs
    in length nubbed == length rs
