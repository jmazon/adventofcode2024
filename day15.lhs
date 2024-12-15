---
title: "AoC Day 15: Warehouse Woes"
author: Jean-Baptiste Mazon
date: 2024-12-15T21:55:32-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: More movement on a 2D grid
image: aoc-haskell.jpeg
---

Movement on a 2D grid!  What a surprise!  In today's [Advent of
Code][aoc] puzzle, “[Warehouse Woes][aoc15]”, we'll be implementing
some variant on a [Sokoban][sokoban] theme.  This is still [literate
Haskell][gh]; here are a few imports.

[aoc]: https://adventofcode.com/
[aoc15]: https://adventofcode.com/2024/day/15
[sokoban]: https://en.wikipedia.org/wiki/Sokoban
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day15.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (forM_,foldM_)
> import Control.Monad.ST (ST,runST)
> import Data.Array.ST (STArray,newListArray,readArray,writeArray,getAssocs)
> import Data.List (find)
> import qualified Data.Set as Set

The grid we're given today comes with a border, which allows us a
refreshingly simple movement operation eschewing bounds checks.

> type Pos = (Int,Int)
> advance :: Pos -> Char -> Pos
> advance (i,j) = \case
>   '<' -> (i,j-1)
>   '>' -> (i,j+1)
>   '^' -> (i-1,j)
>   'v' -> (i+1,j)

I'll keep the parsing operation minimal this time, just separate the
grid from the list of moves.

> parse :: String -> ([String],String)
> parse s = (grid,concat moves) where
>   raw = lines s
>   (grid,"":moves) = break (== "") raw

The conversion from list of line strings to actual 2D grid will
exceptionally be monadic: as I'll be implementing most of the actual
solving in `ST` and part 2 will require preprocessing, this will let
me factor code better.

> linesToArray :: [String] -> ST s (STArray s Pos Char)
> linesToArray grid = do
>   let h = length grid
>       w = length (head grid)
>   newListArray ((0,0),(h-1,w-1)) (concat grid)

So let's now solve!

I'll use the same driver for both parts, parameterized for the two
operations that differ.  The main idea is:

* preprocess and generate the 2D grid as an `STArray`
* locate the starting position
* fold on the provided moves
* sum the boxes' resulting GPS coordinates

The move operation calls `push` to recursively provide the list of
positions to shift, empty if the move doesn't work.  It expects the
player's position shift to come last, so it can pass it on to the next
fold iteration.

> solve :: forall p. Day15 p => ([String],String) -> Int
> solve (grid,moves) = runST $ do
>   arr <- linesToArray (prepare @p grid)
>   start <- maybe undefined fst . find ((== '@') . snd) <$> getAssocs arr
>   let move pos dir = push @p arr pos dir >>= \case
>         [] -> pure pos
>         shifts -> do
>           forM_ shifts $ \(p,p') -> do
>             writeArray arr p' =<< readArray arr p
>             writeArray arr p '.'
>           pure (snd (last shifts))
>   foldM_ move start moves
>   sum . map gps <$> getAssocs arr

Now the part-specific details.  We have two operations.

> class Day15 p where
>   prepare :: [String] -> [String]
>   push :: STArray s Pos Char -> Pos -> Char -> ST s [(Pos,Pos)]

Part 1 works on the raw grid, so preprocessing is a no-op.

> data Part1
> instance Day15 Part1 where
>   prepare = id

Pushing works by recursively checking forward if the target location
is clear.  A wall interrupts recursion with failure; a clear spot ends
recursion with success; a further box continues recursion.

>   push arr = go [] where
>     go acc pos dir = do
>       let pos' = advance pos dir
>           acc' = (pos,pos') : acc
>       readArray arr pos' >>= \case
>         '#' -> pure []
>         '.' -> pure acc'
>         'O' -> go acc' pos' dir

In part 2, the grid is expanded horizontally.

> data Part2
> instance Day15 Part2 where
>   prepare = map . concatMap $ \case
>     '#' -> "##"
>     'O' -> "[]"
>     '.' -> ".."
>     '@' -> "@."

What influence does this have on the push operation?  There's the
following:

* The box is now two characters, who have to succeed their shift
  concurrently for the entire move to be possible.
* We can't get away with simple direct recursion: a same box could now
  be reached by multiple different paths, and we can't have the shift
  operation be returned multiple times, or we'd end up collapsing and
  losing boxes.

I'll implement a variation on [BFS][bfs], where visiting a box half
enqueues the natural target location, but also prepends the other box
half to the queue, thus achieving the forking behavior specific to
today.

[bfs]: https://en.wikipedia.org/wiki/Breadth-first_search

Let's see this part by part.

The interface is the same: take as parameters the array, starting
position and direction to attempt the push.  We initiate the search
with an empty closed set and a single node in the queue: the current
position.  Note the closed set also functions as accumulator to return
the list of shifts, so it consists in two parts: the list to preserve
ordering, and a set for fast lookup.

>   push arr pos0 dir = go ([],Set.empty) [pos0] where

Reaching the end of the queue is a success condition.

>     go (shifts,_) [] = pure shifts

We dequeue position by position, skipping those already visited.

>     go (shifts,set) (pos@(i,j):q)
>       | pos `Set.member` set = go (shifts,set) q

For new visits, we first compute the target location.

>       | otherwise = do
>           let pos' = advance pos dir

For convenience, we factor the computation of the new closed set and
queue before knowing whether we'll be needing them.  Haskell is mostly
lazy[^lazy], this is free.

[^lazy]: The new closed set is fully lazy.  The new queue is sequenced
  through `ST` after the `readArray` operation, that cost is incurred
  no matter what at the Haskell level.  It does have a chance to be
  skipped at a lower level, but it's not significant enough to go and
  check.

>           let cl' = ((pos,pos'):shifts,Set.insert pos set)
>           q' <- readArray arr pos >>= pure . \case
>             _ | dir `elem` "<>" -> q
>             '@' -> q
>             '[' -> (i,j+1) : q
>             ']' -> (i,j-1) : q

The new queue is where we prepend the other half of the box, but only
if pushing it vertically.

And now the recursion cases.  They're almost the same as for part 1,
the differences are merely in accounting for the two possible
characters for half-boxes, and not ending recursion on empty space:
empty space merely validates the current chain, but the other chains
have to be checked too, they're waiting in the queue.

>           readArray arr pos' >>= \case
>             '#' -> pure []
>             '.' -> go cl'  q'
>             '[' -> go cl' (q' ++ [pos'])
>             ']' -> go cl' (q' ++ [pos'])

And that's the meat of it.

Here's the GPS coordinate computing support code.

> gps :: (Pos,Char) -> Int
> gps ((i,j),'O') = 100*i + j
> gps ((i,j),'[') = 100*i + j
> gps _ = 0

And here's a `main` wrapper.

> main :: IO ()
> main = interact $ show . (solve @Part1 &&& solve @Part2) . parse

This concludes today's puzzle.  See you tomorrow!
