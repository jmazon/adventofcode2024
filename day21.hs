import Control.Applicative ((<|>))
import Control.Arrow ((&&&),first)
import Control.Lens ((^.),_1,_2)
import Control.Monad (guard,void,foldM)
import Data.Array (Array,listArray,(!),inRange,bounds,assocs)
import Data.Char (isDigit)
import Data.Function (fix)
import Data.List (find,nub)
import Data.List.Split (split,endsWith)
import Data.Maybe (mapMaybe)
import Data.MemoCombinators (memo3,bool,bits,list,char)
import Linear.V2 (V2(V2))
import qualified Data.Set as Set

type V = V2 Int

newtype Sequence = Sequence String deriving (Eq,Ord,Show)
instance StateMachine Sequence where
  stateGoal (Sequence s) = null s
  stateFollow (Sequence s) k = case s of
    [] -> Nothing
    (h:t) | k == h -> Just ((Sequence t),Nothing)
          | otherwise -> Nothing

data Keypad = Keypad
  { kpPos :: V
  , kpPad :: Array V (Maybe Char)
  }
  deriving (Eq,Ord,Show)

dirpad :: Keypad
dirpad = Keypad dirpadpos dirpadpad where
  dirpadpad = listArray (V2 0 0,V2 1 2)
              [ Nothing, Just '^', Just 'A'
              , Just '<', Just 'v', Just '>'
              ]
  dirpadpos = V2 0 2

codepad :: Keypad
codepad = Keypad codepadpos codepadpad where
  codepadpad = listArray (V2 0 0,V2 3 2)
               [ Just '7', Just '8', Just '9'
               , Just '4', Just '5', Just '6'
               , Just '1', Just '2', Just '3'
               , Nothing,  Just '0', Just 'A'
               ]
  codepadpos = V2 3 2

kpFollow :: Keypad -> Char -> Maybe (Keypad,Maybe Char)
kpFollow kp 'A' = do
  let Just m = kpPad kp ! kpPos kp
  Just (kp,Just m)
kpFollow kp d = do
  let v = case d of
        '^' -> V2 (-1) 0
        '<' -> V2 0 (-1)
        'v' -> V2 1 0
        '>' -> V2 0 1
      p' = kpPos kp + v
  guard (inRange (bounds (kpPad kp)) p')
  void (kpPad kp ! p')
  Just (kp { kpPos = p' },Nothing)

newtype CodePad = CodePad V deriving (Eq,Ord)

bfs :: StateMachine s => [Char] -> s -> Int
bfs is s0 = go Set.empty [(s0,0)] where
  go cl ((p,d):q)
    | p `Set.member` cl = go cl q
    | stateGoal p = d
    | otherwise = go cl' (q ++ q') where
        cl' = Set.insert p cl
        q' = (,d+1) . fst <$> mapMaybe (stateFollow p) is

sampleSequence :: Sequence
sampleSequence = Sequence "029A"

class (Ord s,Show s) => StateMachine s where
  stateGoal :: s -> Bool
  stateFollow :: s -> Char -> Maybe (s,Maybe Char)

instance StateMachine Keypad where
  stateGoal = const False
  stateFollow = kpFollow

data SM = SMKeypad Keypad | SMSequence Sequence deriving (Eq,Ord,Show)
instance StateMachine SM where
  stateGoal (SMKeypad kp) = stateGoal kp
  stateGoal (SMSequence s) = stateGoal s
  stateFollow (SMKeypad kp) i = first SMKeypad <$> stateFollow kp i
  stateFollow (SMSequence s) i = first SMSequence <$> stateFollow s i

instance StateMachine s => StateMachine [s] where
  stateGoal [s] = stateGoal s
  stateGoal (_:ss) = stateGoal ss
  stateFollow (s:ss) i = do
    (s',mbM) <- stateFollow s i
    (ss',m) <- maybe (Just (ss,Nothing)) (stateFollow ss) mbM
    Just (s':ss',m)

main :: IO ()
main = interact $ show . (part1 &&& part2 26) . lines

chain :: Int -> String -> [SM]
chain depth code = replicate depth (SMKeypad dirpad) ++ [SMKeypad codepad,SMSequence (Sequence code)]

part1 :: [String] -> Int
part1 = sum . map complexity where
  complexity code = bfs dirs (chain 2 code) * read (takeWhile isDigit code)

kpFindRaw :: Keypad -> Maybe Char -> V
kpFindRaw kp c = p where Just p = fst <$> find ((== c) . snd) (assocs (kpPad kp))

kpFind :: Keypad -> Char -> V
kpFind kp c = kpFindRaw kp (Just c)

kpFindHole :: Keypad -> V
kpFindHole kp = kpFindRaw kp Nothing

kpPathToPress :: Keypad -> Char -> Char -> [String]
kpPathToPress kp src dst = map (++ "A") $ nub $
  let p1 = kpFind kp dst
      p2 = kpFind kp src
      V2 v h = p1 - p2
      vPath = replicate (abs v) ("^ v" !! (signum v + 1))
      hPath = replicate (abs h) ("< >" !! (signum h + 1))
      hole = kpFindHole kp
  in (vPath ++ hPath) <$ guard ((p1-hole) ^. _1 /= 0 || (p2-hole) ^. _2 /= 0) <|>
     (hPath ++ vPath) <$ guard ((p1-hole) ^. _2 /= 0 || (p2-hole) ^. _1 /= 0)

kpSeq :: Keypad -> [Char] -> [String]
kpSeq kp s =
  foldM (\a bs -> (a ++) <$> bs) [] $
  zipWith (kpPathToPress kp) ('A':s) s

solve :: (Bool -> Int -> String -> Int) -> Bool -> Int -> String -> Int
solve _ _ 0 s = length s
solve rec isCode depth s =
  sum $
  map (
    minimum .
    map (rec False (depth - 1)) .
    kpSeq (if isCode then codepad else dirpad)
  ) $
  split (endsWith "A") s

dirs :: [Char]
dirs = "^<v>A"

part2 :: Int -> [String] -> Int
part2 depth = sum . map complexity where
  complexity :: String -> Int
  complexity code = fix (memo3 bool bits (list char) . solve) True depth code * read (takeWhile isDigit code)
