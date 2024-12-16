import Control.Arrow ((&&&))
import Control.Monad (forM_,foldM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT))
import Data.Array (Array,listArray,bounds,assocs,elems)
import Data.Array.ST (runSTArray,thaw,readArray,writeArray)
import Data.List (find)
import qualified Data.Map.Strict as Map

type Pos = (Int,Int)

main :: IO ()
main = interact $ show . (part1 &&& part2) . parse

parse :: String -> (Array Pos Char,String)
parse s = (listArray ((0,0),(h-1,w-1)) (concat raw),concat moves) where
  raw = lines s
  (grid,"":moves) = break (== "") raw
  h = length grid
  w = length (head grid)

gps :: (Pos,Char) -> Int
gps ((i,j),'O') = 100*i + j
gps ((i,j),'[') = 100*i + j
gps _ = 0

part1 :: (Array Pos Char,String) -> Int
part1 (g,moves) = sum $ map gps $ assocs $ runSTArray $ do
  let Just start = fst <$> find ((== '@') . snd) (assocs g)
  arr <- thaw g
  let push pos@(i,j) dir = do
        what <- readArray arr pos
        let pos' = case dir of
              '<' -> (i,j-1)
              '>' -> (i,j+1)
              '^' -> (i-1,j)
              'v' -> (i+1,j)
        target <- readArray arr pos'
        case target of
          '#' -> pure Nothing
          '.' -> do
            writeArray arr pos' what
            pure (Just pos')
          'O' -> runMaybeT $ do
            MaybeT (push pos' dir)
            lift (writeArray arr pos' what)
            pure pos'
      move pos dir = do
        push pos dir >>= \case
          Nothing -> pure pos
          Just pos' -> do
            writeArray arr pos '.'
            pure pos'
  foldM_ move start moves
  pure arr

expand :: Char -> String
expand '#' = "##"
expand 'O' = "[]"
expand '.' = ".."
expand '@' = "@."

part2 :: (Array Pos Char,String) -> Int
part2 (g,moves) = sum $ map gps $ assocs $ runSTArray $ do
  let (_,(h,w)) = bounds g
      g' = listArray ((0,0),(h,2*w+1)) (concatMap expand (elems g))
  let Just start = fst <$> find ((== '@') . snd) (assocs g')
  arr <- thaw g'
  let push o cl [] = pure (Just (map (id &&& (cl Map.!)) o))
      push o cl ((pos@(i,j),dir):q)
        | pos `Map.member` cl = push o cl q
        | otherwise = do
            what <- readArray arr pos
            let prefix :: [(Pos,Char)] -> [(Pos,Char)]
                prefix | dir `elem` "<>" = id
                       | otherwise = case what of
                  '@' -> id
                  '[' -> (((i,j+1),dir) :)
                  ']' -> (((i,j-1),dir) :)
            let pos' = case dir of
                  '<' -> (i,j-1)
                  '>' -> (i,j+1)
                  '^' -> (i-1,j)
                  'v' -> (i+1,j)
            target <- readArray arr pos'
            case target of
              '#' -> pure Nothing
              '.' -> push (pos:o) (Map.insert pos pos' cl) (prefix q)
              '[' -> push (pos:o) (Map.insert pos pos' cl) (prefix q ++ [(pos',dir)])
              ']' -> push (pos:o) (Map.insert pos pos' cl) (prefix q ++ [(pos',dir)])
      move pos dir = do
        push [] Map.empty [(pos,dir)] >>= \case
          Nothing -> pure pos
          Just shifts -> do
            forM_ shifts $ \(p,p') -> do
              v <- readArray arr p
              writeArray arr p' v
              writeArray arr p '.'
            pure (snd (last shifts))
  foldM_ move start moves
  pure arr
