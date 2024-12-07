---
title: "AoC Day 7: Bridge Repair"
author: Jean-Baptiste Mazon
date: 2024-12-07T14:10:20-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Monads at last!
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] day 7 is “[Bridge Repair][aoc7]”—a nice little
puzzle!

[aoc]: https://adventofcode.com/
[aoc7]: https://adventofcode.com/2024/day/7

Absolutely nothing hard, just a matter of recognizing we have a _very
bounded_ input, neither an $O(2^N)$ nor an $O(3^N)$ algorithm would be
an issue.

Let's start with our typical [literate Haskell][gh] imports.

[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day07.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (foldM,guard)
> import Data.Maybe (mapMaybe)

Parsing.  I'm resisting a strong urge to use `ViewPattern`s here.
You'll thank me later.

So anyway, I parse using the standard library, `lines`, `words` and
`read` for the grunt work, `init` for fancy different number.

> parse :: String -> [(Int,[Int])]
> parse = map f . lines where
>   f l = let (x:xs) = words l
>         in (read (init x),map read xs)

The core search will happen in the list monad, expressing a
computation that has different possible return values.  We'll use it
to return the results of the various pairwise operations we can
perform: sum and product.  We have a nice familiar left-to-right order
of operations.  In other words, a left fold.

So the search is a fold in the list monad over the list of numbers.
We check its result for the presence of the target value, returning
said target wrapped in a `Maybe`.

> search :: (Int -> Int -> [Int]) -> (Int,[Int]) -> Maybe Int
> search op (result,x:xs) = result <$ guard (result `elem` foldM op x xs)

This enables easy checksumming according to the day's formula:

> solve :: (Int -> Int -> [Int]) -> [(Int,[Int])] -> Int
> solve op = sum . mapMaybe (search op)

The operations are the only difference between parts 1 and 2.  We
already evoked part 1's sum and product; for part 2 we add decimal
concatenation, which expresses easily if not fantastically performant.
Not that it matters so much for that input size.

> part1,part2 :: Int -> Int -> [Int]
> part1 a b = [a+b,a*b]
> part2 a b = [a+b,a*b,read (show a ++ show b)]

A wrapper, and it's done.

> main :: IO ()
> main = interact $ show . (solve part1 &&& solve part2) . parse

This concludes today's solution.  See you tomorrow!
