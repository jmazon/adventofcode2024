---
title: "AoC Day 14: Restroom Redoubt"
author: Jean-Baptiste Mazon
date: 2024-12-14T20:58:12-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: It's barely number theory, let's shoehorn it in.
image: aoc-haskell.jpeg
---

Yesterday wasn't number theory, but today's [Advent of Code][aoc]
puzzle, “[Restroom Redoubt][aoc14]”, kind of is.  If you squint.  And
insist.  Let's start, with a few imports to make this post a valid
[literate Haskell][gh] program too.

[aoc]: https://adventofcode.com/
[aoc14]: https://adventofcode.com/2024/day/14
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day14.lhs

> import Control.Arrow ((&&&),(>>>))
> import Data.Char (isDigit)
> import Data.Function (fix)
> import Data.List (genericTake,group,sort)
> import Data.Proxy (Proxy(Proxy))
> import Math.NumberTheory.Moduli (Mod,SomeMod,chineseSomeMod,modulo)
> import GHC.TypeNats (KnownNat,natVal)

So the general idea is we have a bunch of robots evolving on a 2D
grid.  What makes this somewhat number-theoretical is their
teleportation feature, which is a convoluted way of saying their grid
is topologically a torus, wrapping horizontally and vertically.  The
grid dimensions are, conveniently, prime.  Let's define a specialized
vector type for it.

> data V = V
>   { _x :: !(Mod 101)
>   , _y :: !(Mod 103)
>   }

`V2` from the `linear` package resists being parameterized with
different types for its dimensions, so I'll need to scaffold up the
few operations I'll be using.^[Yes, that `Num` instance is very
incomplete.  Doesn't hinder the day.]

> instance Num V where V x1 y1 + V x2 y2 = V (x1+x2) (y1+y2)
> 
> (*^) :: Int -> V -> V
> λ *^ (V x y) = V (fromIntegral λ*x) (fromIntegral λ*y)

Our robots are characterized by a position and a velocity, both
appropriately vectors in this space.

> data Robot = Robot
>   { _pos :: !V
>   , _vel :: !V
>   }

Parsing is arguably the most complicated part of this problem, as
today we do get negatives.  Today I'll enforce a bit of the input's
structure.^[As opposed to yesterday.  For no better reason than
variety.]

> parse :: String -> [Robot]
> parse = map line . lines where
>   line l = Robot (V px py) (V vx vy) where
>     (px,l') = readInt (drop 2 l)
>     (py,l'') = readInt (drop 1 l')
>     (vx,l''') = readInt (drop 3 l'')
>     (vy,[]) = readInt (drop 1 l''')
>   readInt s =
>     let (n,s') = break (not . ((||) <$> isDigit <*> (== '-'))) s
>     in (read n,s')

Our robots know one main operation: advancing in the direction of
their velocity vector.  Let's define an operation to advance them by a
certain number of seconds.

> advance :: Int -> Robot -> Robot
> advance i (Robot pos vel) = Robot (pos + i *^ vel) vel

In part 1, we're asked to advance them by 100 seconds, then compute
some form of checksum on their final positions.

> part1 :: [Robot] -> Int
> part1 = safetyFactor . map (_pos . advance 100) where

Advancing was trivial, but we still need to checksum.

> safetyFactor :: [V] -> Int
> safetyFactor =
>     product . map length . group . sort . filter noEQ . map quadrant
>   where quadrant (V x y) = (compare x 50,compare y 51)
>         noEQ (x,y) = x /= EQ && y /= EQ

Dismissing the crosshairs is mildly annoying, but doable.  So then
we're done for part 1.

In part 2, arguably the most controversial of the year, we're asked to
recognize a christmas tree formed by the robots at a specific moment
in time.

Running for stars, you're mostly doing it visually, guided by various
external knowledge.  Some people had luck minimizing the safety
factor, but it wasn't exactly a slamdunk with my input.^[In my case, I
ended using the “not too many overlapping robots” heuristic, which
wasn't exactly perfect either: a few ealier timestamps shared that
property.]  That doesn't make a good solution post, so let's implement
something reasonable, yet direct.

The main observations for this type of puzzle are:

* axes wrap, so the robots' projection on those axes loop.
* the axes' dimensions are prime^[…and at least one velocity is
  non-null on that axis.], so the loop period is exactly that prime and
  not something smaller.
* the axes' dimensions are coprime, so the full set loops by their
  product.

If the product was actually large, we'd want to detect some form of
“interestingness” per axis, then deduce the peak interestingness
across all axes using the [chinese remainder theorem][CRT].  In this
case, $101×103=10403$ which isn't high enough to not do both at once,
but no matter, let's implement the smart stuff anyway for
demonstration purposes.

[CRT]: https://en.wikipedia.org/wiki/Chinese_remainder_theorem

We still need to find some definition of interestingness.  The [reddit
thread][reddit] has a lot of people using the statistical standard
deviation, but this kind of^[Kind of.  It _is_ fairly reasonable, as
far as AoC goes.]  presupposes to know the shape of the solution
before having seen it, which isn't the most senseful thing ever.  I'll
use the other common approach, measuring [entropy][entropy].

[reddit]: https://old.reddit.com/r/adventofcode/comments/1hdvhvu/2024_day_14_solutions/
[entropy]: https://en.wikipedia.org/wiki/Entropy_(information_theory)

Let's start with the wrapper, for once.  Reading top to bottom, with a
list of robots as input.

> part2 :: [Robot] -> Maybe SomeMod
> part2 =
>   iterate (map (advance 1))                     >>>
>   (map . map) _pos                              >>>
>   (minEntropyOffset _x &&& minEntropyOffset _y) >>>
>   uncurry chineseSomeMod

Generate an infinite list of robot sets, keeping only the positions,
then extract the offset (the index from the start) of the minimal
entropy found when projected to that axis, then combine both axes
using the chinese remainder theorem.

Let's compute the minimal entropy offset now.  This is likely my
niftiest function of the year.  What's so cool about it?  Apart from
the “extractor”, _i.e._ the projection function, all it takes is a
list of vector sets.  It most notably doesn't need the modulo/cycle
length/alphabet size: it takes it from the types at compile time.  I
love it.

> minEntropyOffset :: forall m. KnownNat m => (V -> Mod m) -> [[V]] -> SomeMod
> minEntropyOffset proj =

It uses 3 internal terms: the modulus $m$—a compile-time constant—,
the probability extracting function, and the entropy computing
function.  Note that last one doesn't bother using the appropriate
logarithm base: as we're only comparing those entropies with each
other, that base is only a scaling factor that doesn't influence
ordering.

>   let m = natVal (Proxy @m)
>       prob xs = fromIntegral (length xs) / fromIntegral m
>       entropy p = -p * log p

Now we can process.  Top to bottom: for each set, project to the
relevant axis, sort and group by resulting coordinate; for each
distinct coordinate compute its entropy through probability; infer
global entropy by summing.  Take the first $m$ resulting set
entropies, the rest being expected to cycle.  Extract the index of the
smallest one; then convert to a generic `SomeMod` value so the
library's CRT can operate on simple values.

>   in 
>     map (sum . map (entropy . prob) . group . sort . map proj) >>>
>     genericTake m >>>
>     minIndex >>>
>     flip modulo m

We need a minor helper to extract the index of the smallest.  Nothing
fancy, just recursive pattern matching.

> minIndex :: Ord a => [a] -> Integer
> minIndex (x0:xs0) = flip fix (x0,0,1,xs0) $ \go -> \case
>   (m,mi,i,x:xs) | x < m     -> go (x, i,i+1,xs)
>                 | otherwise -> go (m,mi,i+1,xs)
>   (_,mi,_,[]) -> mi

Adding a `main` wrapper, suspiciously similar to the rest of the
year's.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

And we're done.

Ok, that was really artificial.  But it was pleasant Haskelling, ought
to work for most inputs, and nice code.

See you tomorrow!
