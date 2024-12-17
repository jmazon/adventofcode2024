import Control.Monad
import Control.Monad.RWS.Strict
import Control.Lens
import Data.Bits
import Data.Char
import Data.Maybe
import qualified Data.Vector as V

import Data.SBV
import Data.SBV.Internals (CV(CV),modelAssocs,CVal(CInteger))

data Computer = Computer
  { _cmpA :: !Int
  , _cmpB :: !Int
  , _cmpC :: !Int
  , _cmpIp :: !Int
  }

makeLenses ''Computer

type Program = V.Vector Int
  
parse :: String -> (Program,Computer)
parse s = (V.fromList p,Computer a b c 0) where
  (a:b:c:p) = map read $ words $ mapMaybe simplify s
  simplify ' ' = Just ' '
  simplify ',' = Just ' '
  simplify d | isDigit d = Just d
  simplify _ = Nothing

part1 :: (Program,Computer) -> [Int]
part1 (prg,cmp) = snd $ evalRWS go prg cmp where
  combo o | o < 4 = pure o
  combo 4 = gets _cmpA
  combo 5 = gets _cmpB
  combo 6 = gets _cmpC
  combo 7 = error "Reserved"
  go = do
    ip <- gets _cmpIp
    l <- asks V.length
    when (ip < l) $ do
      instr <- asks (V.! ip)
      operand <- asks (V.! (ip+1))
      case instr of
        0 -> do -- adv
          divisor <- (2^) <$> combo operand
          cmpA %= (`div` divisor)
        1 -> do -- bxl
          cmpB %= xor operand
        2 -> do -- bst
          value <- (`mod` 8) <$> combo operand
          cmpB .= value
        3 -> do -- jnz
          a <- gets _cmpA
          when (a /= 0) $ cmpIp .= operand - 2
        4 -> do -- bxc
          c <- gets _cmpC
          cmpB %= xor c
        5 -> do -- out
          value <- (`mod` 8) <$> combo operand
          tell [value]
        7 -> do -- cdv
          dividend <- gets _cmpA
          divisor <- (2^) <$> combo operand
          cmpC .=  dividend `div` divisor
        i -> error $ "Don't know how to handle instruction " ++ show i
      cmpIp += 2
      go

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ part1 input
  print =<< part2 (fst input)

part2 :: Program -> IO OptimizeResult
part2 prg = do
  let go a (x:xs) = do
        let b1 = (a .&. 7) `xor` 1
            c1 = a `sShiftRight` b1
            b2 = (b1 `xor` c1) `xor` 4
            a1 = a `shiftR` 3
        constrain $ x .== (b2 `sMod` 8)
        go a1 xs :: Symbolic ()
      go a [] = constrain (a .== 0)
  optimize Lexicographic $ do
    initA <- sInt64 "initA"
    minimize "random" initA
    go initA (fromIntegral <$> V.toList prg)
