---
title: "AoC Day 1: Historian Hysteria"
author: Jean-Baptiste Mazon
date: 2024-12-01T15:02:29-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Straightforward implementation
image: aoc-haskell.jpeg
---

Oh dear.  I haven’t posted anything since *two* AoCs ago.  There are
reasons, and I'm still hoping it will change, but…

Anyway.

[Advent of Code][aoc] is back for a new season, and while I'll try and
publish a bit about it, I'll keep the verbosity to a minimum to not
risk last year's situation again.  The first installment, [Historian
Hysteria][aoc1], is a straightforward implementation exercise.  A few
imports to clear out the [literate Haskell][gh].

[aoc]: https://adventofcode.com/
[aoc1]: https://adventofcode.com/2024/day/1
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day01.lhs

> import Control.Arrow ((***),(&&&))
> import Data.List (sort)

The input is provided as two lists of integers, held side-by-side.
Those integers represent so-called _location IDs_.  So in a way, a
large part of the puzzle is getting them parsed.

> type Input = ([Int],[Int])
> 
> parse :: String -> Input
> parse = unzip . pairs . map read . words where
>   pairs (a:b:xs) = (a,b) : pairs xs
>   pairs [] = []

In part 1, we count the total distance between the lists, total
distance being the sum of total distances between pairwise location
IDs from both lists, each considered in ascending order.

> part1 :: Input -> Int
> part1 = sum . uncurry (zipWith ((abs .) . subtract)) . (sort *** sort)

In part 2, we implement a slightly more convoluted operation, a
_similarity score_ representing the sum of products of elements of the
left list by the number of occurences of that same element in the
right list.

> part2 :: Input -> Int
> part2 (ls,rs) = sum $ map (\l -> l * length (filter (== l) rs)) ls

That's it.  A wrapper for completion, and we're done.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This concludes day 1.  See you tomorrow!
