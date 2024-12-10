---
title: "AoC Day 10: Hoof It"
author: Jean-Baptiste Mazon
date: 2024-12-10T06:43:14-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Walking down the contour lines
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] day 10 “[Hoof It][aoc10]”, is a simple little
implementation problem that seems like it can't go wrong brute-force,
but we won't fall for that.  Let's start with our [literate
Haskell][gh] traditional import header.

[aoc]: https://adventofcode.com/
[aoc10]: https://adventofcode.com/2024/day/10
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day10.lhs

> import Control.Arrow ((***),(&&&))
> import Data.Array (Array,listArray,bounds,(!),indices,accumArray,assocs)
> import Data.Char (digitToInt)
> import Data.Foldable (fold,foldl')
> import Data.Ix (Ix,inRange)
> import Data.Monoid (Sum(Sum,getSum))
> import Linear.V2 (V2(V2))
> import qualified Data.Set as Set
> type V = V2 Int

We're given a rectangular^[Or is it square?  It doesn't really
matter.] map of elevations.  We're to find various statistics about
trails, _i.e._ paths going from 0 to 9 in strict increments of 1.

Let's start by parsing the input into a Haskell structure.

> parse :: String -> Array V Int
> parse s = listArray (V2 1 1,V2 h w) (digitToInt <$> concat raw) where
>   raw = lines s
>   h = length raw
>   w = length (head raw)

All requested stats concern points of altitude 0, so we'll gather them
by iterating on contour lines from 9 down.  We'll be extremely generic
here: we'll accumulate on any monoid, starting on `a9`, which is
`mempty` everywhere except on points of elevation 9, where it takes a
generated value of its position instead.

> descend :: Monoid m => (V -> m) -> Array V Int -> Array V m
> descend fromIndex m = foldl' f a9 [8,7..0] where
>   a9 = mapWithIndex (\i x -> if x == 9 then fromIndex i else mempty) m
>   f a h = accumArray (<>) mempty (bounds m)
>           [ (i',a!i)
>           | i <- indices m
>           , d <- [ V2 (-1) 0,V2 0 1,V2 1 0,V2 0 (-1) ]
>           , let i' = i + d
>           , inRange (bounds m) i', m!i' == h ]

The iteration “adds” (in the monoidal sense) the values from level $N$
to the “connected” values of level $N-1$.

This is a bit abstract.  Let's reify.

* In part 1, we're counting the number of reachable summits from any
  given trailhead (_i.e._, trail endpoint of altitude 0).  We'll get
  those by remembering, for each point, the set of distinct summits it
  connects to.  So we'll initialize the monoid with 1-element sets
  containing only that summit (`Set.singleton`), and we'll propagate
  them down using the `Set` `Monoid` instance's default operation
  `Set.union` to keep the distinct list, aggregating over branching
  trails.  We count the summits (`Set.size`) per trailhead.
* In part 2, we're counting the number of paths.  So we initialize
  with unique “ends there” paths (`const 1`) and aggregate by summing.
* In both cases, we report by summing (`Sum` and `fold`).

> solve :: Array V Int -> (Sum Int,Sum Int)
> solve =
>   fold .
>   fmap (Sum . Set.size *** Sum . getSum) .
>   descend (Set.singleton &&& const 1)

Yes, that `Sum . getSum` is very redundant.  Just reproducing the
problem statement, ok?

A `main` wrapper and a missing utility and we're done.

> main :: IO ()
> main = interact $ show . solve . parse
>
> mapWithIndex :: Ix i => (i -> a -> b) -> Array i a -> Array i b
> mapWithIndex f a = listArray (bounds a) $ map (\(i,x) -> f i x) $ assocs a

This concludes today's solution.  See you tomorrow!
