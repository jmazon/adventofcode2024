import Control.Arrow ((&&&))
import Control.Monad
import Data.Array
import Data.List (find,foldl')
import Data.Maybe
import Linear.V2
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type V = V2 Int
type Grid = Array V Char

parse :: String -> Grid
parse s = listArray (V2 1 1,V2 h w) (concat raw) where
  raw = lines s
  h = length raw
  w = length (head raw)

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

part1 :: Grid -> Int
part1 g = go Set.empty (Set.singleton (0,(start,V2 0 1))) where
  Just start = fst <$> find ((== 'S') . snd) (assocs g)
  go cl q = case Set.minView q of
    Nothing -> error "Exit not found"
    Just ((score,state@(pos,_)),q') ->
      -- incorrect (well, wasteful) not to prune again for the closed set
      if g!pos == 'E' then score else
      let cl' = Set.insert state cl
          states = filter ((`Set.notMember` cl) . snd) (expand g (score,state))
          q'' = foldl' (flip Set.insert) q' states
      in go cl' q''

type State = (Int,(V,V))

expand :: Grid -> State -> [State]
expand g (score,(pos,dir)) =
  (score+1000,(pos,perp dir)) :
  (score+1000,(pos,(-perp dir))) :
  let pos' = pos + dir
  in if g!pos' == '#' then [] else [(score+1,(pos',dir))]

expand2 :: Grid -> (State,Maybe (V,V)) -> [(State,Maybe (V,V))]
expand2 g (s@(_,(pos,dir)),_) = map f (expand g s)
  where f s' = (s',Just (pos,dir))

-- this is way too messy
part2 :: Grid -> Int
part2 g = go Nothing Map.empty (Set.singleton ((0,(start,V2 0 1)),Nothing)) where
  Just start = fst <$> find ((== 'S') . snd) (assocs g)
  go :: Maybe (Int,[(V,V)]) -> Map.Map (V,V) (Int,[(V,V)]) -> Set.Set (State,Maybe (V,V)) -> Int
  go best cl q = case Set.minView q of
    Just (((score,state@(pos,_)),prev),q') ->
      case Map.lookup state cl of
        _ | Just (bestScore,bestPoss) <- best, bestScore < score ->
              dfs (snd <$> cl) Set.empty Set.empty bestPoss
        Just (score',prevs)
          | score' < score -> go best cl q'
          | score' == score ->
            let cl' = Map.insert state (score,maybeToList prev ++ prevs) cl
                best' | g!pos == 'E' = (fmap . fmap) (state :) best
                      | otherwise = best
            in go best' cl' q'
          | score' > score -> error "WTF"
        Nothing -> 
          let cl' = Map.insert state (score,maybeToList prev) cl
              states = filter ((`Map.notMember` cl) . snd . fst)
                       (expand2 g ((score,state),prev))
              q'' = foldl' (flip Set.insert) q' states
              best' = (score,[state]) <$ guard (g!pos == 'E')
          in go best' cl' q''
  dfs _ _ acc [] = Set.size acc
  dfs adj cl acc (state@(pos,_):states)
    | state `Set.member` cl = dfs adj cl acc states
    | otherwise =
        let cl' = Set.insert state cl
            acc' = Set.insert pos acc
        in dfs adj cl' acc' (adj Map.! state ++ states)
