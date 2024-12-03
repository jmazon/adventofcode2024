---
title: "AoC Day 3: Mull It Over"
author: Jean-Baptiste Mazon
date: 2024-12-03T07:53:50-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Irregular parsing
image: aoc-haskell.jpeg
---

The [Advent of Code][aoc] day 3 problem “[Mull It Over][aoc3]” is the
first of the year to get us to do a bit of simple parsing.  Of course,
it was quicker to get my stars with two Perl one-liners, but as this
series is mostly Haskell-themed I won't talk too much about that.  On
the other hand, strongly-typed regex-based parsing isn't really
practical, so the imports will revolve around [Megaparsec][mp]
instead.  And they're first, as this is still [literate Haskell][gh].

[aoc]: https://adventofcode.com/
[aoc3]: https://adventofcode.com/2024/day/3
[mp]: https://hackage.haskell.org/package/megaparsec
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day03.lhs

> import Control.Arrow ((&&&))
> import Data.Bool (bool)
> import Data.Void (Void)
> import Text.Megaparsec (Parsec,(<|>),parseMaybe,try,anySingle,many,some)
> import Text.Megaparsec.Char (char,string,digitChar)
>
> type Parser = Parsec Void String

The core of the problem is parsing those `mul(X,Y)` tokens.  That's
two parsers' worth of code.

> int,mul :: Parser Int
> int = read <$> some digitChar
> mul = try $ (*) <$> (string "mul(" *> int <* char ',') <*> int <* char ')'

`int` is straightforward; `mul` could be worth a word:

* the gist is `(*) <$> int <*> int`
* `string` and `char` are there to provide the context.  Crucial, but
  readable.
* parentheses and the rest of the `*>` and `<*` operators are mild
  precedence hacking to ensure the only meaningful arguments to `(*)`
  are the two `int` subparses.
* `try` is there to ensure a partial parse isn't binding.  The input
  is ostensibly hostile; it's not just _likely_ there will be
  occurrences of the string `"mul("` that are not to be parsed as a
  meaningful entry: they're literally right there staring at you in
  the face from the problem statement!

Now indeed we are provided with corrupt input, so the essence of the
challenge is to ignore what isn't parsable.  Most of that will be
accomplished with the `skipAnd` combinator, which parses its argument
if that works, else skips a character and tries again.

> uncorrupt :: Parser a -> Parser [a]
> uncorrupt =
>   let skipAnd p = p <|> anySingle *> skipAnd p

Of course, all of those skipped character are considered as parsed, so
we'll need `try` again to avert failure.

>   in (<* many anySingle) . many . try . skipAnd

`skipAnd` to ignore garbage before any useful input; `try` to prevent
committing; `many` to do so repeatedly and collect the results.

The remaining `(<* many anySingle)` is there to ignore any remainders,
but still consume it all.  That's because we're going to be using the
`parseMaybe` driver, which expects end of input to be reached else it
fails.

We have all we need to package up part 1:

> part1 :: String -> Maybe Int
> part1 = fmap sum . parseMaybe (uncorrupt mul)

In part 2, an additional construct is to be recognized: `don't()`
suspends interpretation until `do()` is encountered.

There are many ways to deal with this.  I chose to just parse them as
additional parsable constructs.  It works because they can't be a part
of the single other parsable construct, `mul()`.

> dodont :: Parser Bool
> dodont =
>   True  <$ string "do()"    <|>
>   False <$ string "don't()"

Ah, but `mul` returns `Int`s, and `dodont` returns `Bool`s.  I'll need
to reconcile them using `Left` and `Right`.

> part2 :: String -> Maybe Int
> part2 =
>   fmap (sum . trigger) .
>   parseMaybe (uncorrupt (Right <$> mul <|> Left <$> dodont))

The parsing part itself returns a stream of `Either Bool Int`.  A
little helper will help us convert that back into a sum of the
relevant part of the `Int`s by returning only those after a `True` and
no `False` yet.

> trigger :: [Either Bool a] -> [a]
> trigger = go True where
>   go p = \case [] -> []
>                (Left p':xs) -> go p' xs
>                (Right x:xs) -> bool id (x :) p (go p xs)

Nothing fancy, just accumulate the input while we're on a `True`
(`bool id (x :)`), updating state when encountering a `Left.`

One more wrapper to conclude the Haskell:

> main :: IO ()
> main = interact $ show . (part1 &&& part2)

All good.

Now did I mention I got my stars with Perl one-liners?

Perl rocks at string processing and native regexes, so the general
idea is to just match all of the `mul\(\d+,\d+\)` we can see, and
compute from there.  Nothing bad can happen, mostly because the
pattern can't interfere with itself.

In my case, where part 2 made this interesting was twofold:

1. how to handle do's and don'ts.  You don't want to blindly erase
anything in-between, because… matching is greedy by default, so
`don't()do()mul(1,1)do()` would ignore everything, while the intent is
to perform that inner `mul`.  So use non-greedy matching
`/don't\(\).*?do\(\)/`
2. but wait, what happens on this: `mul(6,don't()do()7)`?

So theoretically, you don't want to simply just ignore what's between
a don't/do pair, as that risks creating a `mul` that wasn't valid to
begin with in the original input.

So if you go that route, you replace with anything that can't
interfere with a `mul`.

Sadly, it doesn't really matter: it turns out there are no such
occurrences in my input.

What a missed opportunity!
