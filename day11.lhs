---
title: "AoC Day 11: Plutonian Pebbles"
author: Jean-Baptiste Mazon
date: 2024-12-11T07:09:55-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Simple memoization
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] day 11 “[Plutonian Pebbles][aoc11]”, is a simple
little implementation problem that needs a little thought before
cranking up the iterations for part 2.  And a few imports to get the
[literate Haskell][gh] started.

[aoc]: https://adventofcode.com/
[aoc11]: https://adventofcode.com/2024/day/11
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day11.lhs

> import Control.Arrow ((&&&))
> import Control.Monad.State.Strict (State,gets,modify,evalState)
> import qualified Data.Map.Strict as Map

Parsing is dead simple: space-separated integers.

> parse :: String -> [Int]
> parse = map read . words

A single stone gives way to one or two resulting stones, directly as
the problem statement says:

> change :: Int -> [Int]
> change 0 = [1]
> change n =
>   let s = show n in
>   if even (length s)
>   then let (l,r) = splitAt (length s `div` 2) s in [read l,read r]
>   else [n * 2024]

Blinking gets the entire line updated.

> blink :: [Int] -> [Int]
> blink = concatMap change

We solve the problem by counting the resulting stones after 25 blinks.

> part1 :: [Int] -> Int
> part1 = length . (!!25) . iterate blink

For part 2, the resulting number of stones can easily get larger than
I can individually keep track of in RAM.  So we'll stop simulating,
and merely count.  We'll want to memoize on a pair of remaining
iteration count and starting integer, else we'd end up simulating
again, just on the stack instead of a dedicated list structure, which
wouldn't be any better in terms of RAM.

> blink2 :: Int -> Int -> State (Map.Map (Int,Int) Int) Int
> blink2 0 _ = pure 1
> blink2 i n = gets (Map.lookup (i,n)) >>= \case
>   Just r -> pure r
>   Nothing -> do
>     r <- sum <$> mapM (blink2 (i-1)) (change n)
>     modify (Map.insert (i,n) r)
>     pure r

It's semi-noteworthy the first match case (something 0 yields
something 1) looks very similar to the one in the `change` function.
That's a pure coincidence: in `change` it's a given by the problem
statement that a stone marked 0 changes into a stone marked 1; for
`blink2` it merely means no matter what's written on a stone, if we
blink 0 times, it'll result in a count of 1 stone.

> part2 :: [Int] -> Int
> part2 ns = evalState (sum <$> mapM (blink2 75) ns) Map.empty

And we're done for the code.  A little wrapper to invoke it all
concludes it.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This solves today's problem.  See you tomorrow!
