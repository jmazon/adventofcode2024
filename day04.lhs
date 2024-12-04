---
title: "AoC Day 4: Ceres Search"
author: Jean-Baptiste Mazon
date: 2024-12-04T09:49:01-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Grid search
image: aoc-haskell.jpeg
---

On day 4 of [Advent of Code][aoc], “[Ceres Search][aoc4]”, we'll be
implementing a simple word search.  A nice safe problem that seems
like it has no chance of going superlinear no matter how hard we try.

[aoc]: https://adventofcode.com/
[aoc4]: https://adventofcode.com/2024/day/4

Let's start with a few imports, this being [literate Haskell][gh] as
usual.

[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day04.lhs

> import Control.Arrow ((&&&))
> import Data.Array (Array,listArray,(!),bounds,indices,inRange)
> import Linear.V2 (V2(V2))

To warm up, let's first ingest the grid and instil a trace of
structure.

> type V = V2 Int
> type Grid = Array V Char
>
> parse :: [String] -> Grid
> parse ls = listArray (V2 1 1,V2 h w) (concat ls) where
>   (h,w) = (length &&& length . head) ls

Let's start the actual solving with a simple helper, to check whether
a given character is present at a given position on a given grid, with
bounds check.

> hasChar :: Grid -> V -> Char -> Bool
> hasChar grid pos letter = inRange (bounds grid) pos && grid!pos == letter

We'll now extend it to check for an entire word, with an additional
direction vector to search in.

> hasString :: String -> Grid -> V -> V -> Bool
> hasString word grid start dir =
>   and $ zipWith (hasChar grid) (iterate (+ dir) start) word

We can now count the number of times the string "XMAS" starts on a
given position on the grid.

> countXmasAt :: Grid -> V -> Int
> countXmasAt g src =
>   length $ filter (hasString "XMAS" g src) $ V2 <$> [(-1)..1] <*> [(-1)..1]

A little wrapper to sum that over the entire grid, and we're done with
part 1.

> solve :: (Grid -> V -> Int) -> Grid -> Int
> solve f g = sum $ f g <$> indices g

For part2, a few minor changes:

* the word to match is shorter
* we want two distinct matches in a single position for it to count
* anchor is not the word start

> countXMasAt :: Grid -> V -> Int
> countXMasAt grid src = fromEnum $ (== 2) $
>                        length $ filter hasXMas diagonals
>   where hasXMas dir = hasString "MAS" grid (src - dir) dir
>         diagonals = V2 <$> [(-1),1] <*> [(-1),1]

We're using `fromEnum` to convert the `Bool` to an `Int`, as we're
using our `solve` function from part 1 again to sum those over the
grid anyway.

And that's it!

> main :: IO ()
> main = interact $
>   show . (solve countXmasAt &&& solve countXMasAt) . parse . lines

This concludes this day's simple and easy problem.  See you tomorrow!
