---
title: "AoC Day 16: Reindeer Maze"
author: Jean-Baptiste Mazon
date: 2024-12-16T16:05:08-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Yet more movement on a 2D grid
image: aoc-haskell.jpeg
---

More movement on a 2D grid!  What a surprise!  In today's [Advent of
Code][aoc] puzzle, “[Reindeer Maze][aoc16]”, we're playing shortest
paths.  Still [literate Haskell][gh], still a bunch of imports to get
the thing started.

[aoc]: https://adventofcode.com/
[aoc16]: https://adventofcode.com/2024/day/16
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day16.lhs

> import Control.Arrow ((&&&))
> import Data.Array (Array,listArray,assocs,(!))
> import Data.List (find,foldl')
> import Data.Maybe (maybeToList)
> import Data.Semigroup (Min(Min))
> import Linear.V2 (V2(V2),perp)
> import Data.Map.Strict (Map)
> import Data.Set (Set)
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set

Ok, a 2D grid again.  Here's two type synonyms and a `parse` function
you've read quite a few times already.^[I didn't copy-paste a single
one, would you believe it?  My fingers just know the way.]

> type V = V2 Int
> type Grid = Array V Char
>
> parse :: String -> Grid
> parse s = listArray (V2 1 1,V2 h w) (concat raw)
>   where raw = lines s
>         h = length raw
>         w = length (head raw)

So, shortest paths.  This time, actually with different costs per
move, so our good old [BFS][bfs] won't cut it anymore, we'll need full
[Dijkstra][dijkstra].  Our state will simply be position and
orientation.  In our search nodes, we'll want to remember cost, I
mean, score—the one we want to lower—, the current state, and the
state we reached this through.  Score first, as it's the major
Dijkstra order.

[bfs]: https://en.wikipedia.org/wiki/Breadth-first_search
[dijkstra]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

> data State = State { statePos :: !V, stateDir :: !V } deriving (Eq,Ord)
> data Node = Node
>   { nodeScore :: !Int
>   , nodeState :: !State
>   , nodePred :: !(Maybe State)
>   }
>   deriving (Eq,Ord)

Node expansion goes 3-way: 2 turns and 1 step forward.

> expand :: Node -> [Node]
> expand Node{nodeState=nodeState@State{..},..} =
>   [ Node (nodeScore+1000) nodeState { stateDir =       perp stateDir } prev
>   , Node (nodeScore+1000) nodeState { stateDir =      -perp stateDir } prev
>   , Node (nodeScore+   1) nodeState { statePos = statePos + stateDir } prev
>   ]
>   where prev = Just nodeState

I can never get an intuition for which direction `perp` points to in
an indirect system.  Lucky me, it doesn't matter, both turns are
treated equally.

Now here comes the puzzle's complications.

Complication one: the goal state is not known.  I mean, we know the
goal position.  But the position is only one part of the state; the
other being orientation.  And all goal-positioned orientations aren't
necessarily The One.  It usually doesn't matter,[^matters] but in
today's problem we're going to be backtracking down the states, and
the end state we're tracking back from isn't that obvious from a
direction perspective.

[^matters]: This is all the more painful to write down that it really
  doesn't matter as far as my puzzle input goes.  So probably yours
  too.  In multiple temporary states of my rewriting of the code,
  checks for proper goal orientation may have been missing; and yet
  they returned the very same site-blessed result that's worth a
  second star.  Meh.  If I can't justify it, I owe myself the proper
  code, even if the input data isn't up to speed.

Complication two: we're (spoiler alert: for part 2) interested in
_all_ shortest paths.  So we'll need to keep track of them all.  This
makes Dijkstra bookkeeping more “interesting”.  For backtracking
purposes, we want to record which nodes we reached another node from
in its predecessor list—if it's reached at least simultaneously,
obviously—, but we don't really want to expand that node again: any
successor is indistinguishable from the ones we already added to the
queue last time we encountered it.

So the simplicity/correctness compromise I choose is the following:

* encountering a state for the first time obviously makes it A best
  path candidate.
* encountering a state for a non-first time with a strictly higher
  score obviously makes it a skippable dud.
* encountering a state for a non-first time with a score equal to the
  current (and final) best one results in an updated closed set
  including the new node predecessors, and “natural” inclusion in the
  queue of the same successors as the previous time around.
  Rationale: they're not that many to begin with, and they'll be
  dismissed as soon as they're detected.
* we'll explicitly keep track of which goal-positioned states are
  actually final.

So here's the code to determine whether a node should be explored,
returning the new closed set if relevant:

> mbVisit :: Archive -> Node -> Maybe Archive
> mbVisit cl Node{..} = case Map.lookup nodeState cl of
>   Nothing -> Just (Map.insert nodeState (nodeScore,maybeToList nodePred) cl)
>   Just (score,preds)
>     | nodeScore > score -> Nothing
>     | nodeScore == score -> Just $
>         Map.insert nodeState (nodeScore,maybe id (:) nodePred preds) cl

This makes use of our expected Dijkstra closed set.

> type Archive = Map State (Int,[State])

We're now up to implementing Dijkstra proper.  The outer interface is
simple.  As input we have the grid.  As return values, we want:

* the set of states that actually turn out to be goals
* backtracking info per state: score and predecessors

> dijkstra :: Grid -> (Set State,Archive)

As always, we initialize recursion on the starting position and with a
singleton queue.  Not so much as always, we also keep track of which
set of nodes were actually goal nodes (the `Set.empty`).

> dijkstra g = go Set.empty Map.empty q0 where
>   Just start = fst <$> find ((== 'S') . snd) (assocs g)
>   q0 = Set.singleton (Node 0 (State start (V2 0 1)) Nothing)

Par for Dijkstra, the main iteration step looks for the smallest score
available node.

>   go goal cl q = case Set.minView q of

None means the queue is empty, exploration is complete.  We just need
to convert our search-internal data to something the caller actually
wants.  Namely, goal states,^[We don't need full nodes.] and… the
entire actual closed set, whose nodes hold the backtracking
information.

>     Nothing -> (Set.map nodeState goal,cl)

If we're not done, split current node from remaining queue.

>     Just (node,q')

We've been through a lot of trouble identifying which cases were
meaningful.  Invoke those now.  If the closed set updates, the current
node is deemed interesting and explore-worthy.

>       | Just cl' <- mbVisit cl node -> let
>             pos = statePos (nodeState node)

So let's explore it.

As discussed above, whether or not it counts as a goal isn't as
obvious as it looks.  Being located in the end `'E'` cell is the
obvious part, but since we're exploring the entire maze and not
stopping iteration when a shortest path is found,^[Since we're
potentially looking for *all* of them.] we also want to check the
current score is actually on par with the best found so far.

>             goal'
>               | g!pos == 'E' &&
>                 not (any ((< nodeScore node) . nodeScore) goal)
>                 = Set.insert node goal
>               | otherwise = goal

For simplicity, our node expansion function doesn't check for walls,
so we do that here where we have natural access to the grid array,
extending the current queue with current node expansions who don't
walk into a wall.

>             q'' = foldl' (flip Set.insert) q' $
>                   filter ((/= '#') . (g!) . statePos . nodeState) $
>                   expand node

And we can recurse.

>         in go goal' cl' q''

If we're in such a case that's not so meaningful, namely, reaching a
state with a higher score than previously, no biggie, just skip it.

>       | otherwise -> go goal cl q'

The hardest part is done.

In part 1, we want the score of a best path.  Just take the smallest
score of any goal.

> part1 :: (Set State,Archive) -> Min Int
> part1 (goal,m) = foldMap Min . fmap fst $ Map.restrictKeys m goal

By construction, there shouldn't be any difference between any of
them, but I don't want to bother telling them apart.

In part 2, we're interested in the set of positions on a shortest
path.  We'll get the relevant states by [DFS][dfs] and collapse down
to positions.

[dfs]: https://en.wikipedia.org/wiki/Depth-first_search

> part2 :: (Set State,Archive) -> Int
> part2 (goal,m) = go Set.empty (Set.elems goal) where
>   go cl [] = Set.size (Set.map statePos cl)
>   go cl (s:q) | Set.member s cl = go cl q
>               | otherwise = let cl' = Set.insert s cl
>                                 q' = snd (m Map.! s)
>                             in go cl' (q' ++ q)

A trivial `main` function wraps it all.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . dijkstra . parse

This concludes today's solution.  See you tomorrow!
