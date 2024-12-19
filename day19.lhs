---
title: "AoC Day 19: Linen Layout"
author: Jean-Baptiste Mazon
date: 2024-12-19T09:34:55-01:00
tags: [ "advent of code", aoc2024, haskell, perl ]
description: That was refreshing.
image: aoc-haskell.jpeg
---

Refreshingly, no 2D grid today.  For [Advent of Code][aoc]'s 19^th^
day, the “[Linen Layout][aoc19]” puzzle asks us to to arrange complex
stripe patterns from smaller pieces.  This is [literate Haskell][gh],
introduced by an import block.

[aoc]: https://adventofcode.com/
[aoc19]: https://adventofcode.com/2024/day/19
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day19.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (forM_)
> import Control.Monad.ST (ST,runST)
> import Data.Array.ST (STArray,newArray,readArray,writeArray)
> import Data.List (tails)
> import Data.Maybe (mapMaybe)

Parsing is rather simple: we have the smaller pieces comma-separated
on the first line; then come the target designs.

> parse :: String -> ([String],[String])
> parse s = (patterns,towels) where
>   (rawPatterns:_:towels) = lines s
>   patterns = words (filter (/= ',') rawPatterns)

Part 1 asks us which designs are achievable using the source material.

This is exactly the kind of thing [regular expressions][re] are for.
So this is exactly the kind of task I don't run to Haskell for,
reaching out for [Perl][gha] instead.

[re]: https://en.wikipedia.org/wiki/Regular_expression
[gha]: https://github.com/jmazon/adventofcode2024/blob/master/day19a.pl

``` Perl
my @patterns = split /, /, <>;
say 0+ grep /^(@{[join '|', @patterns]})+$/o, <>;
```

Line 1 parses the comma-separated patterns; line 2 counts the number
of remaining lines that match the following regex:

* anchored in start `^` and end `$` of line
* composed of one or more `+` of the enclosed group `()`^[The
  one-or-more hack allows me to skip special-casing the separation
  line between patterns and designs.  I'm a golfer at heart.]
* containing the `|`-joined patterns, spliced into the regex string
  using the babycart operator `@{[]}`

In part 2, we're asked for the number of ways to achieve the
arrangements.  To the best of my knowledge., this is not something the
common regex engines easily spit out.

So let's use [DP][dp] instead.

[dp]: https://en.wikipedia.org/wiki/Dynamic_programming

The main problem is: how many ways can the patterns be arranged to
result in the given design?

It can be split into the following subproblem: how many ways can the
patterns be arranged to result in any given prefix of the given
design?

This can be solved recursively:

* the empty prefix can be constructed in exactly one way: by using
  exactly zero patterns.
* the number of ways any prefix can be constructed can be computed by
  checking which patterns match the end of the prefix, and summing the
  number of ways to construct the prefix _without_ the matching
  pattern.  Which happens to necessarily also be a prefix of the same
  string.

Since the dependency only goes towards shorter prefixes, we can
simplify and proceed forward only by shifting the summing operation to
the operation of the shorter prefix:

* we maintain an array of known counts for each prefix length
* we move left to right, checking at every position which patterns
  match
* for every match, we increase the array one pattern length further by
  the count at the current position
* when we're done, the answer is in the rightmost array position

This translates directly to Haskell:

> part2 :: ([String],[String]) -> [Int]
> part2 (patterns,towels) = map dp towels where
>   dp towel = runST $ do
>     let l = length towel
>     a <- newArray (0,l) 0 :: ST s (STArray s Int Int)
>     writeArray a 0 1
>     forM_ (zip [0..] (tails towel)) $ \(i,suffix) -> do
>       cur <- readArray a i
>       forM_ (mapMaybe (match suffix) patterns) $ \l' -> do
>         let i' = i + l'
>         prev <- readArray a i'
>         writeArray a i' (prev + cur)
>     readArray a l

…with a little helper to compute `isPrefixOf` and `length`
simultaneously:

> match :: String -> String -> Maybe Int
> match = go 0 where
>   go i (s:string) (p:prefix)
>     | s == p = go (i+1) string prefix
>     | otherwise = Nothing
>   go i _ [] = Just i
>   go _ _ _ = Nothing

This computes the solution in a time proportional to the size of the
patterns times the size of the designs, which is a lot better than
naïve tree traversal.^[On particularly adversarial inputs, you'd want
to match all patterns more concurrently at a given point, _e.g._ using
a [trie][trie], so the total complexity would depend on the length of
the largest pattern instead of their combined sum of lengths.]

[trie]: https://en.wikipedia.org/wiki/Trie

While we're here, let's implement part 1 in Haskell too, as a special
case of part 2.

> part1 :: ([String],[String]) -> Int
> part1 = length . filter (> 0) . part2

And a little wrapper to bind it all.

> main :: IO ()
> main = interact $ show . (part1 &&& sum . part2) . parse

This concludes today's solution.  See you tomorrow!
