---
title: "AoC Day 13: Claw Contraption"
author: Jean-Baptiste Mazon
date: 2024-12-13T07:01:28-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: It's not number theory
image: aoc-haskell.jpeg
---

No number theory for today.  In [Advent of Code][aoc], day 13, “[Claw
Contraption][aoc13]”, we're merely going to exercies very basic 2D
linear algebra.  Writing as little as possible, so many [literate
Haskell][gh] imports.

[aoc]: https://adventofcode.com/
[aoc13]: https://adventofcode.com/2024/day/13
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day13.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (guard)
> import Data.Char (isDigit)
> import Data.List.Split (wordsBy)
> import Data.Ratio (Ratio,numerator,denominator)
> import Data.Maybe (listToMaybe)
> import Data.Monoid (Sum(Sum))
> import Linear.Matrix (luSolveFinite,transpose)
> import Linear.V2 (V2(V2))
> import Linear.Vector ((*^))
> 
> type V = V2 Int

Let's make parsing trivial.  All claw increments are forward or to the
right, so there's no need to parse any sign.  All we need to do is
extract numbers and group them by pairs into vectors then by triplets
into puzzle cases.

> parse :: String -> [(V,V,V)]
> parse = triplets . pairs . wordsBy (not . isDigit) where
>   pairs (a:b:xs) = V2 (read a) (read b) : pairs xs
>   pairs [] = []
>   triplets (a:b:c:xs) = (a,b,c) : triplets xs
>   triplets [] = []

Cases are independent, we just need to sum in the end, so we'll just
solve them directly and sum in the wrapper.

In part 1, we're only interested in claw solutions with 100 moves or
less per axis.  100×100 is small, we can afford to search it all.

> part1 :: (V,V,V) -> Maybe (Sum Int)
> part1 (a,b,g) = listToMaybe $ do
>   u <- [0..100]
>   v <- [0..100]
>   guard (g == u *^ a + v *^ b)
>   pure (Sum (3*u + v))

But wait, you (should) complain, this merely gives us the solution
with the smaller press count of the A button, what if there's a better
solution with more B?

Well… that's not possible in my input set: the direction vectors for A
and B presses are never collinear there.

This makes part 2 just as easy: since the two vectors aren't
collinear, there's always a single solution over rational pairs.  So
we can just solve for that, ensure it's an actual solution, _i.e._
reduced denominators are both 1, and report.  My input doesn't even
have a case where the solution would be invalid because of a negative
coordinate on one axis.

> part2 :: (V,V,V) -> Maybe (Sum Int)
> part2 (a,b,g0) = Sum (3*u + v) <$ guard ((du,dv) == (1,1)) where
>   g = fromIntegral <$> g0 + V2 10000000000000 10000000000000
>   m = fmap fromIntegral <$> V2 a b
>   V2 (u :% du) (v :% dv) = luSolveFinite (transpose m) g

I'm using the library solver for the sake of it; the 2D case is
unidirectional arithmetic anyway.

Let's add the little support code to make the above readable.

> pattern (:%) :: Int -> Int -> Ratio Int
> pattern n :% d <- (numerator &&& denominator -> (n,d))
> {-# COMPLETE (:%) #-}

And a `main` wrapper.

> main :: IO ()
> main = interact $ show . (foldMap part1 &&& foldMap part2) . parse

We're done.

See you tomorrow!
