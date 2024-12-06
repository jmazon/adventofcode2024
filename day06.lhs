---
title: "AoC Day 6: Guard Gallivant"
author: Jean-Baptiste Mazon
date: 2024-12-06T09:58:40-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Bruteish grid walking
image: aoc-haskell.jpeg
---

If I'm not mistaken, [Advent of Code][aoc] day 6, “[Guard
Gallivant][aoc6]”, is the first grid walker of the year.  Grid walker.
What kind of a category is that?  Well, It's common enough in AoC.
Let's walk!  Starting with a few imports, for the [literate
Haskell][gh] aspect of it.

[aoc]: https://adventofcode.com/
[aoc6]: https://adventofcode.com/2024/day/6
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day06.lhs

> import Control.Monad.ST (ST,runST)
> import Data.Array (Array,listArray,bounds,indices,inRange,(!),(//))
> import Data.Array.ST (STArray,getAssocs,newArray,readArray,writeArray)
> import Data.Function (fix,on)
> import Data.List (delete,find,groupBy)
> import Linear.V2 (V2(V2))
> 
> type V = V2 Int

You'll excuse me for sneaking a type along with the imports.  Anyway,
the input is one of those typical 2D grids.  In addition to mapping it
all to an array, we'll isolate the starting position `^`.  The
starting direction is always “up”, no need to get any more fancy than
needed there.

> parse :: String -> (Array V Char,V)
> parse s = (grid,startPos) where
>   raw = lines s
>   h = length raw
>   w = length (head raw)
>   grid = listArray (V2 1 1,V2 h w) (concat raw)
>   Just startPos = find ((== '^') . (grid!)) (indices grid)

I'm using a sinple 0–3 integer to encode direction.^[I've been through
a lot more while solving for this, but this was my best compromise.]
$0$ is the starting “up” direction, and there's only one possible
operation on them, namely… well, you get the picture.

> dirs :: Array Int V
> dirs = listArray (0,3) [V2 (-1) 0,V2 0 1,V2 1 0,V2 0 (-1)]
> 
> turnRight :: Int -> Int
> turnRight = (`mod` 4) . succ

Let's now implement the workhorse.

Oh wait.  I haven't talked about _that_ yet, have I?

The big deal today is following the path from that `^` starting
position, walk straight ahead until we ever encounter a `#`, in which
case we bump and turn right.  Hence the function above.

So let's implement the actual walking.

Which such a setup, there's only two reasonable outcomes.

> data Result a = Exit a | Loop

Let's patrol this out!

> patrol :: Patrol a => Array V Char -> V -> a
> patrol grid startPos = runST $ do
>   let startDir = 0
>       (a,b) = bounds grid

As we said earlier, the starting direction is known and constant.
I'll implement the patrolling in a standard graph-traversal style,
with a closed set to remember where we've been before and avoid
looping.

>   closed <- newArray ((a,0),(b,3)) False

The rest is your standard traversal, nothing specific but the daily
special of turning right when bumping into something.

>   flip fix (startPos,startDir) $ \loop (p,v) -> do
>     prev <- readArray closed (p,v)
>     if prev
>       then report Loop
>       else do
>         writeArray closed (p,v) True
>         let p' = p + dirs!v
>         if (inRange (bounds grid) p')
>           then if grid!p' == '#'
>                then loop (p,turnRight v)
>                else loop (p',v)
>           else report (Exit closed)

I used a typeclass to weasel my way out of making it clear what that
patrolling function should actually return.

> class Patrol a where
>   report :: Result (STArray s (V,Int) Bool) -> ST s a

In part 1, we want to know the number of grid positions the guard will
cover while patrolling.

So what I'm interested in is the set (reified as a list because I'm
merely interpreting the traversal's “closed” set) of walked positoins.

> instance Patrol [V] where
>   report (Exit a) =
>     map (fst . fst . head)
>     . filter (or . map snd)
>     . groupBy ((==) `on` (fst . fst))
>     <$> getAssocs a

Those composed `fst` and `snd` are a bit of a pain.  The gist is: we
parse the closed set (`getAssocs a`), group them by first index
(namely position; the second index as implemented above would be
direction), keep only those that have actually been patrolled through
(`or . map snd`), keep an actual position out of it (`fst . fst`).

Looping in part 1 shouldn't happen, so I'm logging an error there for
branch completeness's sake, but it isn't used in real life.

>   report Loop = error "Loop."

That's it, right?  A bit of wrapping, and we solve part 1 with flying
colors!

> main :: IO ()
> main = do
>   (grid,startPos) <- parse <$> getContents
>   let pathPoss = patrol grid startPos
>   print (length pathPoss)

For part 2, we square out the complexity.  We're counting the number
of grid positions we can drop an obstacle on, that would result in the
guard entering a loop.  So we'll use that `report` function again,
this time returning a simple boolean out of it (reproduced later as
our literate Haskell is currently still in the middle of `main`).

The number of positions we could drop an obstacle that would even be
noticed by the guard isn't that much smaller than the total grid size.
About 25% in my input's case.  Bah, we've computed it, might as well
re-use it.

>   let obstaclePoss = delete startPos pathPoss

The main operation from part 1 becomes an iteration step in part 2:

>   let hasLoop obstaclePos = patrol (grid // [(obstaclePos,'#')]) startPos

And that's it!  Report it and the star is ours.

>   print $ length $ filter hasLoop obstaclePoss
>
> instance Patrol Bool where
>   report (Exit _) = pure False
>   report Loop = pure True

This concludes day 6's problem.  See you tomorrow!
