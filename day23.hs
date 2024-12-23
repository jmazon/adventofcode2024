import Control.Arrow ((***),(&&&))
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

type Computer = String
data Graph = Graph
  { computers :: ![Computer]
  , connections :: !(Set (Computer,Computer))
  }
type Clique = [Computer]

parse :: String -> Graph
parse = uncurry Graph . (Set.elems . Set.unions *** Set.unions) .
        unzip . map parseEdge . lines
  where parseEdge [a1,b1,'-',a2,b2] =
          let c1 = [a1,b1]
              c2 = [a2,b2]
          in (Set.fromList [c1,c2],Set.fromList [(c1,c2),(c2,c1)])

extend :: Graph -> Clique -> [Clique]
extend g [] = pure <$> computers g
extend g cl@(c:_) = do
  c' <- takeWhile (< c) (computers g)
  guard $ all (\c'' -> (c',c'') `Set.member` connections g) cl
  pure (c':cl)

cliques :: Graph -> [[Clique]]
cliques g = iterate (concatMap (extend g)) [[]]

part1 :: [[Clique]] -> Int
part1 = length . filter (any ((== 't') . head)) . (!! 3)

part2 :: [[Clique]] -> String
part2 = intercalate "," . head . last . takeWhile (not . null)

main :: IO ()
main = interact $ show . (part1 &&& part2) . cliques . parse
