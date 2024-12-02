---
title: "AoC Day 2: Red-Nosed Reports"
author: Jean-Baptiste Mazon
date: 2024-12-02T01:01:45-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: More early advent point-free fun
image: aoc-haskell.jpeg
---

For [Advent of Code][aoc] day 2, “[Red-Nosed Reports][aoc2]”, we'll
perform various forms of grid analysis.  But first a few imports to
clear out the [literate Haskell][gh] situation.

[aoc]: https://adventofcode.com/
[aoc2]: https://adventofcode.com/2024/day/2
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day02.lhs

> import Control.Arrow ((&&&))
> import Data.List (inits,tails)

I said grid.  So let's parse a grid.  Of `Int`s.

> parse :: String -> [[Int]]
> parse = (map . map) read . map words . lines

Ok, grid.  Rows are “reports”; columns are “levels”.  The point of the
day is to count how many reports are “safe”, a report's safety being a
function of its levels.  Let's quickly define a little helper.

> countIf :: (a -> Bool) -> [a] -> Int
> countIf = (length .) . filter

Mmmm, [point-free notation][pf].  A hard drug, if you ask me.  Better
saved for the most casual occurrences of programming.  Which is the
case for AoC.

[pf]: https://en.wikipedia.org/w/index.php?title=Point-free_programming

Anyway, what does it mean for a report to be safe?  Paraphrasing the
statement, it's safe if its levels are [monotonic][monotonic], with a
sequential pairwise difference exactly between 1 and 3.  In point-free
style:

[monotonic]: https://en.wikipedia.org/wiki/Monotonic_function

> isSafe :: [Int] -> Bool
> isSafe =
>   uncurry (||) .
>   (($ between 1 3) &&& ($ between (-3) (-1))) .
>   flip all .
>   (zipWith subtract <*> tail)

This may be a bit much at once.  Starting with the type signature,
we're reading a list of `Int`s, a report of levels, and outputing a
`Bool`, that level's safety.

Reading from the end, `zipWith subtract <*> tail` is one of those
classic abuses of Haskell default typeclass instances.  In this case,
a `Reader` instance on functions.  Expanding on that isn't really the
point of this post^[But could be the point of a separate one, if
there's demand for such pointless fun.], just see it as a concise way
of duplicating the use of the provided argument.  Or as an
[S combinator][s], if you're into [lambda calculus][lc].  Anyway, as a
whole it performs pairwise subtraction.

[s]: https://en.wikipedia.org/wiki/SKI_combinator_calculus#Informal_description
[lc]: https://en.wikipedia.org/wiki/Lambda_calculus

Then `flip all` checks all of those deltas pass some predicate
function, yet to be provided.

The last two lines are a common Arrow idiom.  The first (lower) line
is an especially common^[Particularly in AoC, where we just about
always apply *two* functions to some same input.] abuse, sorry,
pattern, of Arrow notation, where instead of just applying a function
to the chain, we want to apply *two*, and get a pair in return
instead.  Conversely, the second line, `uncurry (||)`, unpacks that
pair to apply the logical or function.  So, in effect, those two lines
return the logical or of two functions.

Its a matter of taste as to which of the `uncurry`/`(&&&)` pattern and
the `liftA2` one look better.  I personally don't have a strong
preference, and oscillate a lot between the both.

We still need a little helper, that's not yet provided by the standard
library.

> between :: Int -> Int -> Int -> Bool
> between a b x = a <= x && x <= b

A little wrapper, very point-free as well, and we're done.

> main :: IO ()
> main = interact $ show . (countIf isSafe &&& countIf isSafe') . parse

Oh no.  I forgot about part 2.  Surprise!  A new definition for
safety.  Now a safe report is allowed to drop a single arbitrary
level.  For redundancy or something.  Let's have a function for that.

> leaveOneOut :: [a] -> [[a]]
> leaveOneOut = zipWith (++) . inits <*> tail . tails

We take a list, and return a list of similar lists, dropping all
individual elements from it one by one.  In point-free syntax again.

You ought to recognize:

* the use of `<*>` to use the input argument twice, in this case the
  list.  Precedence might not be obvious.  `<*>` binds looser than
  `(.)`, so the function really is `(zipWith (++) . inits) <*> tail
  . tails`
* `inits` and `tails` are standard library functions that yield the
  list of a list's suffixes and prefixes, respectively.  They
  complement each other quite well, in such a manner `zip inits <*>
  tails` returns each input list's element exactly once per row.
* squeezing a `tail` next to that `tails` drops the first suffix, so
  we're now matching each prefix with the next suffix.  In effect,
  skipping one element.
* instead of simple `zip`, we `zipWith (++)`, _i.e._ concatenate the
  list portions back together.

And that may have been the most interesting part of the day's problem.
Let's get back to boring detail and actually implement the new safety
check, making good re-use of the version from part 1.

> isSafe' :: [Int] -> Bool
> isSafe' = any isSafe . ((:) <*> leaveOneOut)

Yeah, I couldn't resist a little more point-free abuse.  How does this
work?  Mostly, we're calling `leaveOneOut` on the input report,
yielding a list of incomplete-by-one reports, and checking if `any` of
them `isSafe`.

But that would miss the case where the report is already safe by
part 1 standards.  So we include that one as well, by combining
(“consing” `(:)`) the untouched input report with the `leaveOneOut`ed
ones.  S combinator magic again.

That's it for day 2.  You wouldn't abuse point-free so much in real
life, but I have to confess all that AoC does make me appreciate
lambda bindings the rest of the year.

See you tomorrow!
