---
title: "AoC Day 5: Print Queue"
author: Jean-Baptiste Mazon
date: 2024-12-05T10:27:11-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Toposhmological
image: aoc-haskell.jpeg
---

We're still on [Advent of Code][aoc], and today's puzzle, “[Print
Queue][aoc5]”, is rather interesting, in that it was very good at
doing something I typically adore AoC for even though it caught me by
surprise way more times than I'm willing to admit: tilting the scales.

[aoc]: https://adventofcode.com/
[aoc5]: https://adventofcode.com/2024/day/5

How was it surprising?  It's a matter of expectations.

A significant^[Which isn't necessarily to say “sizable”.] portion of
the attendance is programming competition regulars.  Those are people
looking for competition, familiar with a few formats close to that of
AoC.  They're trained for competing, are good at it, they're the
people you typically see on top of the daily leaderboard, as well as
other programming competitions.

AoC has blatant similarities with the usual (programming competition)
suspects, but has a notable difference that Eric was always quite
vocal in defending: the problem is individual.

What does that mean?  In a typical programming competition, there is a
problem statement, an expectation you're to write some code to convert
some input into some output, usually judged as a crude function to a
very small codomain: it's either correct or it isn't.  The statement
gives (or tends to, at least) some real-world storytelling about a
down-to-earth math problem.  A “limits” or “constraints” section is a
common staple, giving you a more numerical expectation of what's to be
expected from the input.

And that's the important part.  You're given an expectation of what to
expect from the input.  But you're not ^[With
[notable](https://en.wikipedia.org/wiki/Google_Code_Jam) exceptions.]
_given_ that input.  The input will be the typical worst case of what
to expect to remain compliant with both the problem statement and the
constraints.

That's not the case in AoC.  In AoC, the goal is explicitly to solve
for your puzzle input.  Which may involve actually looking at your
problem input.

So, the problem statement never^[AFAIK.] lies, and neither does the
ground-truth problem input; yet either are quite commonly misleading.
_I.e._, the problem input for a given problem statement is very
commonly not a worst case.

We'll see exactly by how much after the code.  Until then, let's have
our usual [literate Haskell][gh] imports.

[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day05.lhs

> import Data.Graph (graphFromEdges,reverseTopSort)
> import Data.List (partition)
> import Data.List.Split (wordsBy)
> import qualified Data.IntMap as Map
> import qualified Data.IntSet as Set

So.

We're given a list of page-to-page precedence constraints and a [list
of] list of pages, and asked to ensure any such list respects the
precedence constraints.  Any CS major will tell you this screams
[topological sort][ts] all over.

[ts]: https://en.wikipedia.org/wiki/Topological_sorting

What's a topological sort?  Nothing more than an ordering that
satisfies given pairwise ordering constraints.

For part 1, we're not that deep in CS theory yet.  We're only to
assert whether a given ordering actually satisfies the constraints.

How do we do that?  Let's try for as greedy as possible: what does it
take, given an assumed-valid prior input, to accept a new page to the
end of the list?

Page constraints are pairwise, so adding a new page can only have two
sides: either that new page is a problem as the predecessor side, or
it is as the successor side.  But it can't be a predecessor-side issue
yet, as there's no successor to conflict with yet.  What conflict
could it possibly get into on the successor side?

Adding a page to a presumed-valid ordering can break if that new page
is present in the constraint ordering as a successor of a page that is
already a part of the (presumed valid) ordering.  So adding that page
to the ordering would create a (successor,predecessor) pair that is
invalid.

So that's a path to an algorithm.

We can greedily read a page number at a time, accepting it only if it
is never a predecessor to a pair ordering whose successor we've
already seen in the list.  In other words, we can maintain a set of
forbidden pages, initially empty.  So we can only add a page to the
ordering if it's not a predecessor to a page we already saw.  And
after adding it, we can henceforth exclude any predecessor it might be
known to have.

Which brings us to input parsing.  We'll want to know what known
predecessors a given successor page has, so we'll aggregate those as a
`Set`.  The list of test cases is parsed as a list, no surprise there.

> parse :: String -> (Map.IntMap Set.IntSet,[[Int]])
> parse s = (rules,updates) where
>   [rawRules,rawUpdates] = wordsBy (== "") (lines s)
>   parseRule l = (b,Set.singleton a) where [a,b] = read <$> wordsBy (== '|') l
>   rules = Map.fromListWith Set.union (parseRule <$> rawRules)
>   updates = map read . wordsBy (== ',') <$> rawUpdates

The problem workhorse, assessing orderlyness.  Implemented, as said,
as a forward scan.  We're adding pages one by one, only rejecting if
the new page introduces an incompatibility.  A page can never
introduce an incompatibility as a predecessor, so we only keep track
of pages in a sucessor relationship, lest we notice their partner
predecessor later along the way.

> isOrdered :: Map.IntMap Set.IntSet -> [Int] -> Bool
> isOrdered rules = go Set.empty where
>   go _ [] = True
>   go forbidden (p:ps)
>     | p `Set.member` forbidden = False
>     | otherwise = go forbidden' ps where
>         forbidden' = Set.union forbidden $
>                      Map.findWithDefault Set.empty p rules

We're keeping those orderings that validate, and keeping their middle
element as proof.

> middle :: [a] -> a
> middle l = l !! (length l `div` 2)

This solves part 1.

For part 2 we'll want to _correct_.

What's interesting is the problem asks for “the” correct ordering's
middle element.  And while there existing _some_ correct orderings
isn't too surprising given the form of the inputs, it's not as obvious
they should all have the same middle element so as to be recognized by
the site puzzle logic.

Yet… we're allowed to assume good faith.  The problem statement asks
for something; it's not directly a problem for that requested
something to be seemingly impossible to generate input data for, as
long as the web interface accepts my result data.

So I'll leave the structure of the input as an unexplored rabbit hole
for today, but do encourage you to explore it on your own.

Back to solving.  One “correct” way to go is definitely the page pair
set's topologial ordering we mentioned earlier, more or less by
definition.  As we already parsed the input as successor to
predecessor relationships, so we might as well use that and use a
reverse topological sort ordering instead.

I'm not going to expand on the actual algorithm for topological sort:
it seems like it's so boring it actually made it to the Haskell
standard library by now.

> makeOrdered :: Map.IntMap Set.IntSet -> [Int] -> [Int]
> makeOrdered rules update = map keyFromVertex (reverseTopSort (graph)) where
>   updateSet = Set.fromList update
>   mkEdges page =
>     ( ()
>     , page
>     , Set.elems $
>       Set.intersection updateSet $
>       Map.findWithDefault Set.empty page rules
>     )
>   (graph,nodeFromVertex,_) = graphFromEdges $ map mkEdges update
>   keyFromVertex v = let (_,key,_) = nodeFromVertex v in key

And there you have it.  A solved day 5.  Only lacking a trivial
wrapper to get things going our way:

> main :: IO ()
> main = do
>   (rules,updates) <- parse <$> getContents
>   let (ordered,unordered) = partition (isOrdered rules) updates
>   print $ sum $ middle <$> ordered
>   print $ sum $ middle . makeOrdered rules <$> unordered

Thank you for following along, and see you tomorrow!
