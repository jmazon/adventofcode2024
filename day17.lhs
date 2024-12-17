---
title: "AoC Day 17: Chronospatial Computer"
author: Jean-Baptiste Mazon
date: 2024-12-17T13:35:37-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: This was nice.
image: aoc-haskell.jpeg
---

No movement on a 2D grid!  There was a lot of that in this year's
[Advent of Code][aoc], but today's puzzle “[Chronospatial
Computer][aoc17]” doesn't have any.  Instead, forward and backward
propagation along a simple CPU's time axis.  No exception, this post
doubles as a [literate Haskell][gh] program, which is why we have all
of those imports here to get started.

[aoc]: https://adventofcode.com/
[aoc17]: https://adventofcode.com/2024/day/17
[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day17.lhs

> import Control.Lens (makeLenses,ASetter',(^.),(&),(.~),(%~))
> import Data.Char (isDigit)
> import Data.List (genericIndex,genericLength)
> import Data.Maybe (mapMaybe)
> import Data.SBV
> import Data.SBV.List (implode,nil,snoc)

Our computer and its program are 3-bit.

> type Word3 = WordN 3
> type Program = [Word3]

Its registers, though, are theoretically unbounded.  As far as my
input's concerned, I only really need 48 bits for A, 3 bits for B and
6 for C.  Let's keep it simple and homogeneous.

> type SRegister = SWord64
> data Computer = Computer
>   { _regA    :: !SRegister
>   , _regB    :: !SRegister
>   , _regC    :: !SRegister
>   , _outList :: !(SList Word3)
>   , _starved :: !SBool
>   }
> makeLenses ''Computer

I'm storing the computer's output along in the same structure.  Don't
worry too much about that “starvation” parameter, that's matter for
part 2.

So apparently we have either literal or combo arguments.  That second
kind is worth a dedicated function.

> combo :: Computer -> Word3 -> SRegister
> combo _ o | o < 4 = fromIntegral o
> combo c 4 = c ^. regA
> combo c 5 = c ^. regB
> combo c 6 = c ^. regC

The `fromIntegral` here is a conversion from literal to symbolic
value.

Oh yes, I almost forgot to mention.

I'm implementing today's problem as a symbolic model.  Which isn't
anything notable for part 1, but will allow a bit of magic in part 2.
With the `Word3` shenanigans, it does mean I'll be using
`fromIntegral` a *lot*, and for different purposes.  I'll try and
specify which is which when it happens.

We've got the operands, let's now deal with the operations themselves.

> type SimpleOp = Computer -> Word3 -> Computer
> adv,bxl,bst,bxc,out,bdv,cdv :: SimpleOp

The first one, `adv`, happens to share quite a lot with the last two,
`bdv` and `cdv`.

> [adv,bdv,cdv] = xdv <$> [regA,regB,regC]

They're all some form of division.  Looking closer, they're division
by two to the power of some other integer value.  This would be a
right shift if we knew we were always dealing with positives.

Are we?

We are.^[For my input.]  By virtue of the only sign-sensitive
operation being that division, and since all [of my] initial register
values are positive, for all intents and purposes all registers will
only ever be positive.

So we can implement with a bitwise shift.

> xdv :: ASetter' Computer SRegister -> SimpleOp
> xdv regLens c operand =
>   let dividend = c ^. regA
>       bits = combo c operand
>       result = dividend `sShiftRight` bits
>   in c & regLens .~ result

The rest of the operations all write to the B register for some
reason.  The `fromIntegral` call here comverts from `Word3` to
`Word64`.^[Not that B ever needs more than 3 bits, but that much is
input-dependent to the best of this solution's knowledge.]

> bxl c operand = c & regB %~ xor (fromIntegral operand)
> bst c operand = c & regB .~ combo c operand .&. 7
> bxc c _op     = c & regB %~ xor (c ^. regC)

The output operation is slightly more involved, but `SBV` nicely
supports lists and a `snoc` operation that does the heavy lifting.
The integral-related function call is not actually `fromIntegral` here
but `sFromIntegral`, which converts from SRegister to SWord3 (thus
obviating the need to mod 8 or bitwise-and 7 as the statement would
otherwise require).

> out c operand = c & outList %~
>   flip snoc (sFromIntegral (combo c operand))

The remaining operation touches the instruction pointer, so I'll
implement it inline.  Here's now the main computer flow, implemented
as a simple recursive process.  Ignore the starvation-related lines,
they're not relevant yet.  The `fromIntegral` call in the `jnz` code
converts from `Word3` to the addressible program space, which, to the
best of my reading of the statement, is unbounded.^[Though I'm still
casting to `Int`.  Honestly, all that matters is that it's more than
3-bit wide so the program-end condition $ip \ge l$ can tested properly
without too many bit-safe contorsions.]

> computerRun :: Program -> Computer -> Symbolic Computer
> computerRun prg c0 = do
>   let ops = [adv,bxl,bst,undefined,bxc,out,bdv,cdv]
>   let go c ip _  | ip >= genericLength prg = c
>       go c _  bb | bb <  0                 = c & starved .~ sTrue
>       go c ip bb =
>         let opcode  = genericIndex prg  ip
>             operand = genericIndex prg (ip + 1)
>             ip' = ip + 2
>         in if opcode == 3 -- jnz
>            then ite (c ^. regA ./= 0)
>                   (go c (fromIntegral operand) (bb - 1))
>                   (go c               ip'       bb)
>            else go (genericIndex ops opcode c operand) ip' bb
>   let c' = go c0 (0 :: Int) (15 :: Int)
>   minimize "starved" $ c' ^. starved
>   pure c'

In part 1, we just want to run the computation forward.  So we
constrain the output list at flow end to the free variable, and solve
(trivially).  This is invoked as an optimization problem for
consistency, but it's as good as sat.

> part1 :: (Program,Computer) -> IO OptimizeResult
> part1 (prg,computer) = optimize Lexicographic $ \output -> do
>   computer' <- computerRun prg computer
>   solve [output .== (computer' ^. outList)]

In part 2, we're looking for the lowest A register input that could
yield the program code as output.  So we update the initial computer
state with the free variable, and constrain the output list to match
the program source.  The `fromIntegral` here converts the program's
immediate `Word3` to symbolic ones.

> part2 :: (Program,Computer) -> IO OptimizeResult
> part2 (prg,c0) = optimize Lexicographic $ \a0 -> do
>   c' <- computerRun prg (c0 & regA .~ a0)
>   constrain $
>         c' ^. outList .== implode (fromIntegral <$> prg)
>     .|| c' ^. starved
>   minimize "a0" a0

How this can work at all is maybe worth a word.  Reversing a
computation is famously known not to be feasible in the general case.
After all, that's why cryptography makes such central use of [one-way
functions][owf].  Why would this one be any different?

[owf]: https://en.wikipedia.org/wiki/One-way_function

Well, individual compute steps are still reversible; what makes it
intractable at scale is the branching factor.  And the branching
factor is noticeably low with the sort of programs that came as inputs
to today's puzzle: in my case it's a simple loop deconstructing the
initial contents of the A register.  So, in effect, simply repeating
the program a few times.

To avoid a human needing to peek at the code, I'm trying to solve this
as generically as possible, so I won't unroll the loop.  But Z3 still
needs to trace variables across the branch point, the `ite` statement
in the `computerRun` function.  And those would introduce an infinite
tree of computations if left to themselves.

To avoid that, I introduced the `bb` parameter, short for “branching
budget”, that is decreased every time the jump is effective.  After
the budged is depleted, the computation is declared “starved” and
aborted.  This enforces a finite tree, at the cost of potentially
missing solutions.  They'll be reported with the `starved` variable
set to 1, which the general program strives to avoid anyway (it's the
first minimized parameter in the symbolic computation), which is a
hint to the operator the budget ought to be increased.

Which circles back nicely to tractability: at some point you're not
going to have enough resources to keep increasing that budget, and
this is likely to coincide with computations that are hard to reverse.
There is no guarantee you'll get an answer at all: the system can't
predict how long the computation would run in the general
case.[^halting]

[^halting]: Such a prediction is known to be impossible in the general
  case: that's the infamous [halting problem][halting].  Do note that
  it's defined for [Turing machines][tm].  It's unobvious at first
  glance whether the computer defined here is [Turing-complete][tc].
  Weak arguments in favor: there's a bit of conditional branching and
  a bit of computation.  Strong arguments against: Branching is
  limited to 8 addresses, storage is more or less limited to the size
  of the initial contents of the A register, and a far cry from
  random-access.  Still not a proof.  This post treats it as
  generically as a Turing machine, and that means the branching budget
  is undecidable, _i.e._ left to the operator.

[halting]: https://en.wikipedia.org/wiki/Halting_problem
[tm]: https://en.wikipedia.org/wiki/Turing_machine
[tc]: https://en.wikipedia.org/wiki/Turing_completeness

Note how I accept starved computations as solutions: if I didn't do
this, we'd get starved computations reported as “unsatisfiable”, which
wouldn't tell us whether throwing more branching budget at it had a
chance.  With this in place, we have three possible outcomes:
satisfied (_i.e._, solved), unsatisfiable (_i.e._, it's proven that no
solution exists), and satisfied where the solution is reported as
starved, which we can safely interpret as: “undecided within the
bounds of the alloted budget”.

I delayed the parsing.  Let's include that now.

> parse :: String -> (Program,Computer)
> parse s = (program,computer) where
>   (a:b:c:p) = map read $ words $ mapMaybe simplify s
>   program = fromIntegral <$> p
>   computer =
>     Computer (fromIntegral a) (fromIntegral b) (fromIntegral c) nil sFalse
>   simplify ' ' = Just ' '
>   simplify ',' = Just ' '
>   simplify d | isDigit d = Just d
>   simplify _ = Nothing

There's two distinct cases of `fromIntegral` here: the one for
`program` converts to `Word3`; the ones for `computer` convert to
symbolic variables.

Almost there.  The `main` wrapper is trivial enough.

> main :: IO ()
> main = do
>   input <- parse <$> getContents
>   print =<< part1 input
>   print =<< part2 input

And we need a bit of glue code to merge on the `Computer` structure.
(It's a requirement of the symbolic if statement.)  We'd get them for
free had we used simple tuples, but code readability has a cost.

> instance Mergeable Computer where
>   symbolicMerge force t (Computer a1 b1 c1 o1 s1) (Computer a2 b2 c2 o2 s2) =
>     Computer
>       (symbolicMerge force t a1 a2)
>       (symbolicMerge force t b1 b2)
>       (symbolicMerge force t c1 c2)
>       (symbolicMerge force t o1 o2)
>       (symbolicMerge force t s1 s2)

This concludes today's puzzle!  See you tomorrow!
