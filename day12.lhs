---
title: "AoC Day 12: Garden Groups"
author: Jean-Baptiste Mazon
date: 2024-12-12T21:36:35-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Planar union-find
image: aoc-haskell.jpeg
---

Today's [Advent of Code][aoc], “[Garden Groups][aoc12]”, is mostly
about grouping plots of land into patches, so a typical union-find
problem.  Let's start with the relevant imports, this being [literate
Haskell][gh].

[aoc]: https://adventofcode.com/
[aoc12]: https://adventofcode.com/2024/day/12
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day12.lhs

> import Control.Arrow ((***),(&&&))
> import Control.Monad (forM_,when)
> import Control.Monad.ST (ST,runST)
> import Control.Monad.Trans (lift)
> import Data.Array (Array,listArray,bounds,(!),indices,inRange)
> import Data.Array.ST (STArray,newArray,modifyArray',freeze)
> import Data.Equivalence.Monad (runEquivT,equate,classDesc)
> import Data.Semigroup (Min(Min,getMin),Sum(Sum,getSum))

Parsing is more or less the same as every second day.

> type Grid = Array (Int,Int)
> 
> parse :: String -> Grid Char
> parse s = listArray ((1,1),(h,w)) (concat raw) where
>   raw = lines s
>   h = length raw
>   w = length (head raw)

Now the meat of the day is grouping the plots.  Part 1 asks for some
computation based on perimeters, so we'll be storing those too.  Both
parts ask for something related to areas, so we'll store those as
well.  Part 2 asks for something more complicated, so we'll also
remember a patch identifier of sorts.  (That happens to be the first
encountered position coordinates involved in the group.)

> type Processed = (Grid Int,Grid (Int,Int),Grid Int)

We trigger the process using the `equivalence` package's initializer
`(Min i,Sum 1)` representing the group representative's coordinages
and an individual plot's area, so we can use the standard semigroup
appending operation `(<>)` as a merge operation.

> process :: Grid Char -> Processed
> process g = runST $ runEquivT (\i -> (Min i,Sum 1)) (<>) $ do

For perimeters, we'll maintain an external array.  It starts
initialized at 4, meaning each square plot has 4 surrounding units of
fence.

>   ps <- lift (newArray (bounds g) 4 :: ST s (STArray s (Int,Int) Int))

We'll then define a merge operation, that we'll invoke on each pair of
adjacent plots.  If the conditions are met, namely both plots are of
the same type of plant, we'll merge them in the `equivalence`
structure as well as decrease both sides' fences by the one between
the both.

>   let maybeMerge u v = when (g!u == g!v) $ do
>         equate u v
>         lift $ modifyArray' ps u pred
>         lift $ modifyArray' ps v pred

We can now group all relevant plots.

>   let ((t,l),(b,r)) = bounds g
>   forM_ [t..b] $ \i ->
>     forM_ [l..r] $ \j -> do
>       when (j < r) $ maybeMerge (i,j) (i,j+1)
>       when (i < b) $ maybeMerge (i,j) (i+1,j)

We'll extract identifiers and areas as separate arrays for easier
external consumption, and return it all.

>   (identifiers,areas) <-
>     ( listArray (bounds g) . map getMin ***
>       listArray (bounds g) . map getSum
>     ) .
>     unzip <$>
>     mapM classDesc (indices g)
>   perimeters <- lift (freeze ps)
>   pure (perimeters,identifiers,areas)

In part 1, the requested computation is the sum per patch of product
of area by perimeter, which we can express more or less directly.

> part1 :: Processed -> Int
> part1 (perimeters,_,areas) = sum (price <$> indices areas)
>   where price u = (areas ! u) * (perimeters ! u)

Part 2 is trickier: it asks for the product of area by number of
sides.  Sides are slightly messy, but they're dual and equal in
cardinality to corners, which we can compute easily, if not a bit
verbosely.

The general idea is to go plot by plot.  A given plot has a convex
corner if two adjacent plots are not a part of the same patch.  A
given plot is a non-convex corner if two adjacent plots are a part of
the same patch, but the diagonal one in-between is not.

It's barely worth it to factor out the side pairing, so it results in
2×4 cases.

> part2 :: Processed -> Int
> part2 (_,identifiers,areas) = sum (discountedPrice <$> indices areas)
>   where discountedPrice (i,j) = areas!(i,j) * corners
>           where sameShape v =
>                   inRange (bounds areas) v &&
>                   (identifiers!v == identifiers!(i,j))
>                 [ nw,n,ne
>                  , w,_, e
>                  ,sw,s,se
>                  ] = map sameShape
>                        [(i-1,j-1),(i-1,j),(i-1,j+1)
>                        ,(i,  j-1),(i,  j),(i,  j+1)
>                        ,(i+1,j-1),(i+1,j),(i+1,j+1)]
>                 corners =
>                   fromEnum (not n && not w) +
>                   fromEnum (not n && not e) +
>                   fromEnum (not s && not w) +
>                   fromEnum (not s && not e) +
>                   fromEnum (n && w && not nw) +
>                   fromEnum (n && e && not ne) +
>                   fromEnum (s && w && not sw) +
>                   fromEnum (s && e && not se)

A little wrapper to invoke it all.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . process . parse

This concludes today's puzzle.  See you tomorrow!
