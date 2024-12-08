---
title: "AoC Day 8: Resonant Collinearity"
author: Jean-Baptiste Mazon
date: 2024-12-08T08:57:05-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Simple implementation
image: aoc-haskell.jpeg
---

Next up, [Advent of Code][aoc] gives us “[Resonant
Collinearity][aoc8]” for day 8.  Retrospectively, slightly
disappointing, as there's… really nothing to it, we'll just implement.
Here's our imports, to keep up the [literate Haskell][gh] vibe.

[aoc]: https://adventofcode.com/
[aoc8]: https://adventofcode.com/2024/day/8
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day08.lhs

> import Control.Monad (guard)
> import Data.Array (Array,listArray,assocs,bounds,inRange)
> import Data.Function (on)
> import Data.List (delete,groupBy,nub,sort)
> import Data.Maybe (mapMaybe)
> import Linear.V2 (V2(V2))
> 
> type V = V2 Int

Most of the story here is actually the parsing.  Let's first ingest
the grid as such.

> parse :: String -> Array V Char
> parse s = listArray (V2 1 1,V2 h w) (concat raw) where
>   raw = lines s
>   h = length raw
>   w = length (head raw)

Now we really want to know where the antennas are.  Grouped by
frequency.

> nodeGroups :: Array V Char -> [[V]]
> nodeGroups =
>   (map . map) snd .
>   groupBy ((==) `on` fst) .
>   sort .
>   mapMaybe (\(i,e) -> (e,i) <$ guard (e /= '.')) .
>   assocs

It reads bottom-to-top as per usual point-free notation: pair up grid
indices with antenna frequency if any, keep only those with an actual
antenna there, sort by frequency, group by frequency, keep only
indices.

Now solving for the problem is a simple matter of counting distinct
antinode positions.

> solve :: Array V a -> ((V,V) -> [V]) -> [[V]] -> Int
> solve g antinodes =
>   countUnique .
>   concatMap
>     ( concatMap (takeWhile (inRange (bounds g)) . antinodes) .
>       pairs
>     )

Ok, ok, that's a mouthful.  From a high level, we're operating per
frequency.  For each of those, we receive a list of coordinates.  We
generate all ordered pairs from them, and the list of induced
antinodes from there, stopping when they step out of grid bounds.

We merge them all cross-frequency, and count the resulting distinct
positions.

This uses two utterly generic helpers:

> pairs :: Eq a => [a] -> [(a,a)]
> pairs xs = [ (x,x') | x <- xs, x' <- delete x xs ]
>
> countUnique :: Eq a => [a] -> Int
> countUnique = length . nub

Now how do we induce antinodes?  That much depends on the problem half
we're dealing with.

> part1,part2 :: (V,V) -> [V]

In part 1, the next antinode from a node is the point of symmetry over
its counterpart.

> part1 (a,b) = [b+b-a]

In part 2, the antinodes form a (dotted) line extending infinitely (to
the point of remaining on the grid) past the counterpart.

> part2 (a@(V2 x1 y1),V2 x2 y2) = iterate (+v) a

This is actually the part where the puzzle was mildly disappointing.
Paraphrasing the problem statement, we want “every grid position
exactly in line”.  In the normal world, In the normal world, this
means the very simple following grid…

    A.A

…should have the following list of antinodes:

    ###

Because they're all exactly on the same horizontal line.  Right?

Well, maybe that's the case.  But it's not present in my input, and
neither is it in that of anyone I asked.  So my countermeasures are
entirely useless.^[But not unjustified.]

>   where
>     d = gcd (x2-x1) (y2-y1)
>     v = V2 ((x2-x1) `div` d) ((y2-y1) `div` d)

Oh well.

One thing to observe in the^[I won't claim credit, I got the hack from
browsing other people's solutions after getting my two stars.]
implementation, though, is the reason for using _ordered_ pairs.  They
allow me to factor the antinodes' bounds checking in a way that
maximizes code re-use between parts 1 and 2.  Not exactly
life-changing, but makes for a nicer write-up.

Anyway, that's about it by now, right?

> main :: IO ()
> main = do
>   g <- parse <$> getContents
>   let nodes = nodeGroups g
>   print $ solve g part1 nodes
>   print $ solve g part2 nodes

This concludes today's solution.  See you tomorrow!
