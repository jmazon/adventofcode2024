import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Data.Array
import Data.Array.ST
import Data.Equivalence.Monad

main :: IO ()
main = do
  raw <- lines <$> getContents
  let h = length raw
      w = length (head raw)
      g = listArray ((1,1),(h,w)) (concat raw)

  print $ runST $ runEquivT (const 1) (+) $ do
    ps <- lift (newArray (bounds g) 4 :: ST s (STArray s (Int,Int) Int))
    let maybeMerge u v = when (g!u == g!v) $ do
          equate u v
          lift $ modifyArray' ps u pred
          lift $ modifyArray' ps v pred
          
    forM_ [1..h] $ \i ->
      forM_ [1..w] $ \j -> do
        when (j < w) $ maybeMerge (i,j) (i,j+1)
        when (i < h) $ maybeMerge (i,j) (i+1,j)

    let price u = do
          area <- classDesc u
          perimeter <- lift (readArray ps u)
          pure (area * perimeter)

    totalPrice <- sum <$> mapM price (indices g)

    let discountedPrice (i,j) = do
          area <- classDesc (i,j)
          [ nw,n,ne
           , w,_, e
           ,sw,s,se
           ] <- mapM (equivalent (i,j))
                 [(i-1,j-1),(i-1,j),(i-1,j+1)
                 ,(i,  j-1),(i,  j),(i,  j+1)
                 ,(i+1,j-1),(i+1,j),(i+1,j+1)]
          let corners =
                fromEnum (not n && not w) +
                fromEnum (not n && not e) +
                fromEnum (not s && not w) +
                fromEnum (not s && not e) +
                fromEnum (n && w && not nw) +
                fromEnum (n && e && not ne) +
                fromEnum (s && w && not sw) +
                fromEnum (s && e && not se)
          pure (area * corners)
    
    totalDiscountedPrice <- sum <$> mapM discountedPrice (indices g)

    pure (totalPrice,totalDiscountedPrice)
