---
title: "AoC Day 20: Race Condition"
author: Jean-Baptiste Mazon
date: 2024-12-20T09:58:26-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Meh.
image: aoc-haskell.jpeg
---

A shortest path problem on a 2D grid.  Color me surprised.  Today's
problem, “[Race Condition][aoc20]”, isn't anything drastically new as
far as [Advent of Code][aoc] goes.  No matter, we'll solve it
nonetheless.  Starting with a few imports, since this is as [literate
Haskell][gh] as ever.

[aoc]: https://adventofcode.com/
[aoc20]: https://adventofcode.com/2024/day/20
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day20.lhs

> import Control.Arrow ((&&&))
> import Data.Array (Array,listArray,bounds,(!),indices,inRange,range)
> import Data.Array.ST (runSTArray,newArray,readArray,writeArray)
> import Data.List (find)
> import Linear.V2 (V2(V2))
>
> type V = V2 Int

The puzzle input is a 2D grid.  I've stopped counting how many times
my `parse` function was *this* for the year.

> parse :: String -> Array V Char
> parse s = listArray (V2 1 1,V2 h w) (concat raw) where
>   raw = lines s
>   h = length raw
>   w = length (head raw)

So, we want to count 100-cheats.  What's an N-cheat?  It's a
singularity in the map where skipping across a wall once would result
in a total race time that's better than the normal race time by $N-2$.
$-2$ because it still does take two time units to walk across the
wall.

> solve :: [V] -> Array V Char -> Int
> solve jumps g =

The easiest way I know to check for cheats is to know, for every
position on the grid, how far it is from the goal.  This way, we can
identify good cheats by simply comparing distances-to-goal of both
cheat start and end point.

Let's first locate the endpoint.

>   let
>     Just goalPos = find ((== 'E') . (g!)) (indices g)

We can then construct those distances to goal by simple [BFS][bfs].

[bfs]: https://en.wikipedia.org/wiki/Breadth-first_search

>     dists = runSTArray (newArray (bounds g) maxBound >>= bfs [(goalPos,0)])

>     bfs [] a = pure a
>     bfs ((p,d):q) a = do
>       prev <- readArray a p
>       enqueue <- if prev < maxBound
>                  then pure id
>                  else do
>                    writeArray a p d
>                    pure $ flip (++)
>                         $ map (,d+1)
>                         $ filter ((/= '#') . (g!))
>                         $ filter (inRange (bounds g))
>                         $ map (p +) dirs
>       bfs (enqueue q) a

The precise formula to evaluate a cheat is the gain in distance minus
the time consumed by the cheat itself.  It's always 2 in part 1, but
let's keep it generic.  Of course, cheats are only defined from actual
racetrack positions, in other words, any place not a wall.

>     eval p d
>       | g!p == '#' = minBound
>       | p' <- p+d =
>           if | not (inRange (bounds g) p') -> minBound
>              | g!p' == '#' -> minBound
>              | otherwise -> dists!p - dists!p' - manhattan (p'-p)

We'll call that with an externally-provided list of jump vectors, so
the bounds checks are performed inside.

The list of cheat [values] can then be defined on the cartesian
product of positions by possible jumps.

>     cheats = eval <$> indices g <*> jumps

We'll have all of the invalids in there as `minBound`.  They aren't
exactly a problem as we're filtering for the top anyway.

>   in length (filter (>= 100) cheats)

The valid jumps take us from a race position to any other valid race
position 2 units in any direction, provided the destination is also a
valid race position, and the intermediate position is a wall.

> jumps1 :: [V]
> jumps1 = map (2 *) dirs

Sorry, no, that was too complicated.  If the destination isn't a valid
race position, its distance to the goal as computed by our BFS will
remain `maxBound`, so it doesn't hold a good chance of standing out,
and there's no need to check for that.

Similarly, it the intermediate position isn't a wall, well, cheating
doesn't make a difference from not cheating.  In a very literal
meaning: the gain in skipping collision detection for 2 picoseconds
will be zero, so those cheats are worth exactly that.  We could debate for a
long time whether or not that makes them valid cheats, but for all
practical purposes, they're not going to pass the N-cheat test and
hence be an issue worthy of further consideration.

Part 2 extends the cheat time.  So we extend the cheat range.

> jumps2 :: [V]
> jumps2 = let r = 20 in
>   filter ((<= r) . manhattan) (range (V2 (-r) (-r),V2 r r))

That's the gist of it.  Let's add a support function and a useful
constant list.

> manhattan :: V -> Int
> manhattan = sum . abs
>
> dirs :: [V]
> dirs = [V2 (-1) 0,V2 0 1,V2 1 0,V2 0 (-1)]

And a wrapper.

> main :: IO ()
> main = interact $ show . (solve jumps1 &&& solve jumps2) . parse

I think that's it.

See you tomorrow!
