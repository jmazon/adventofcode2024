---
title: "AoC Day 18: RAM Run"
author: Jean-Baptiste Mazon
date: 2024-12-18T09:36:34-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: This was easy.
image: aoc-haskell.jpeg
---

Back to 2D grids.  In today's [Advent of Code][aoc] puzzle “[RAM
RUN][aoc18]”, the input isn't one, but it becomes a part of the spirit
straight away.  Let's resist and not include anything array-related in
our imports.  Which still come first, this being [literate
Haskell][gh].

[aoc]: https://adventofcode.com/
[aoc18]: https://adventofcode.com/2024/day/18
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day18.lhs

> import Control.Arrow ((&&&))
> import Control.Lens ((??),(^.),view)
> import Control.Monad (forM_,when)
> import Data.Equivalence.Monad (runEquivM',equate,equivalent)
> import Data.Function (fix)
> import Data.Ix (inRange)
> import Linear.V2 (V2(V2),_x,_y)
> import qualified Data.Set as Set
> 
> type V = V2 Int

The input is actually a list of coordinates.  We'll be operating on a
prefix of them in part 1, so a list is actually a reasonable structure
to hold them in.  In addition, we'll guess the grid size from the
contents—this is one of those annoying puzzles where the examples in
the statement don't work with the code that solves the player's
input.^[The bounds-extracting method isn't exactly foolproof either,
though it works on both the statement example data and my input.  I
take the min and max of both coordinates of every byte.  But there's
no guarantee they're going to result in the starting and ending
position.  The only constraints I read from the statement are that the
first 1024 bytes don't block the way, and the full input does.
Without the 1024-byte lower bound, the trivial counterexample is an
input consisting of a single byte `0,0`.  But we can still pack a
$70^2=4900$-byte square in a corner without reaching the right column,
for example.  FWIW, my input isn't as long as that.]

> parse :: String -> ((V,V),[V])
> parse = (getBounds &&& id) . map readV . lines where
>   readV l = V2 (read x) (read y) where (x,_:y) = break (== ',') l
>   getBounds xys =
>     ( V2 (minimum (view _x <$> xys)) (minimum (view _y <$> xys))
>     , V2 (maximum (view _x <$> xys)) (maximum (view _y <$> xys)) )

Part 1 is our usual shortest path.  I'll implement it with a simple
[BFS][bfs], taking care not to reify the RAM to a 2D array—a symbol of
my resistance to the year's underlying theme.

[bfs]: https://en.wikipedia.org/wiki/Breadth-first_search

I have it compute the appropriate prefix length by checking the grid
dimensions.  Which also validates we got them right.

> part1 :: ((V,V),[V]) -> Maybe Int
> part1 ((startPos,endPos),ps) = bfs Set.empty [(startPos,0)] where
>   threshold = case endPos of V2  6  6 ->   12
>                              V2 70 70 -> 1024
>   corrupted = Set.fromList (take threshold ps)
>   valid p = inRange (startPos,endPos) p && p `Set.notMember` corrupted
>   bfs _ [] = Nothing
>   bfs cl ((p,d):q)
>     | p == endPos = Just d
>     | p `Set.member` cl = bfs cl q
>     | otherwise = bfs cl' (q ++ q') where
>         cl' = Set.insert p cl
>         q' = map (,d+1) $
>              filter (`Set.notMember` cl) $
>              filter valid (neighborsManhattan p)

Part 2 is a weird one.  It's trivial to solve by binary search over
prefix length using the code from part 1.  (In fact it seems
reasonable to even search using _linear_ search.)

That's what I did at the time, anyway.

But it's very inefficient for the type of problem this has become.
We're not interested in the shortest path length anymore, merely if
one exists.  Which is the exact same problem as checking whether _a_
path exist, no matter its length—if one or more exists, one of them
will be the shortest, but it won't change the number of bytes to
obstruction.

So this is a mere connectedness problem.  A path exists as long as we
keep adding bytes to the picture that do not connect the top and right
“walls” to the bottom and left ones.

This yields a simple implementation using the `equivalence` package.

> part2 :: ((V,V),[V]) -> V
> part2 ((V2 top left,V2 bottom right),ps0) =
>   runEquivM' $ flip fix (Set.empty,ps0) $ \loop (corrupted,p:ps) -> do
>     forM_ (neighborsChebyshev p) $ \p' ->
>       when (p' `Set.member` corrupted) $ equate (Right p) (Right p')
>     let topRight = Left True
>         bottomLeft = Left False
>     when (p ^. _x == top || p ^. _y == right) $ equate (Right p) topRight
>     when (p ^. _x == bottom || p ^. _y == left) $ equate (Right p) bottomLeft
>     equivalent topRight bottomLeft >>= \case
>       False -> loop (Set.insert p corrupted,ps)
>       True -> pure p

Sadly, this is longer than binary search over part 1.  Weird moral
here.

Let's add a bit of convoluted support code to make it all work.

> neighborsManhattan,neighborsChebyshev :: V -> [V]
> neighborsManhattan p = ([(+),(-)] ?? p) <*> [V2 1 0,V2 0 1]
> neighborsChebyshev p = map (p +) $ V2 <$> [-1..1] <*> [-1..1]

And a trivial wrapper.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

We're all done!  See you tomorrow!
