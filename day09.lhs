---
title: "AoC Day 9: Disk Fragmenter"
author: Jean-Baptiste Mazon
date: 2024-12-09T12:56:02-01:00
tags: [ "advent of code", aoc2024, haskell ]
description: Could've been simple implementation if we didn't want optimal.
image: aoc-haskell.jpeg
---

Most promising [Advent of Code][aoc] title in a long time.  And the
puzzle behind, “[Disk Fragmenter][aoc9]”, is the source of even more
fun than the title let through.

[aoc]: https://adventofcode.com/
[aoc9]: https://adventofcode.com/2024/day/9

On the surface, it's simple implementation.  All of my acquaintances
who talked to me about it did the same reasonable thing as me and
implemented it quadratically.  And the subset of those “who knew” had
the same feeling of unease about doing it that way, yearning for the
linearithmic way, but not really wanting to spend that much time to
get it right.

We'll get it right [enough] in this post, starting with a few imports
to justify all the [literate Haskelling][gh].

[gh]: https://github.com/jmazon/adventofcode2024/blob/master/day09.lhs

> import Control.Arrow ((&&&))
> import Control.Monad (when)
> import Data.Array (Array)
> import Data.Array.ST (runSTArray,getBounds,newListArray,writeArray,readArray)
> import Data.Char (digitToInt)
> import Data.Foldable (toList,foldl')
> import Data.Semigroup (Max(Max))
> import Data.FingerTree hiding (reverse)

Let's define a pair of useful types.  First a `FileId` type synonym,
mostly to distinguish it from size and index `Int`s.

> type FileId = Int
> data Chunk = Chunk
>   { chunkWidth :: !Int
>   , chunkFile :: !(Maybe FileId)
>   }
>   deriving Eq

Then a `Chunk`, which can represent either a file or empty space,
depending on the `Maybe` within.

Our parsing will create those directly from the compressed disk map
we're given, generating file ids along the way.

> parse :: String -> [Chunk]
> parse str = chunks where
>   diskMap = map digitToInt str
>   chunks = go 0 True diskMap where
>     go i  True (s:ss) = Chunk s (Just i) : go (i+1) False ss
>     go i False (s:ss) = Chunk s  Nothing : go  i     True ss
>     go _    _    []   =                 []

For code corectness concerns later on, it's worth noticing:

* It's legitimate for there to be a 0-block empty chunk as a separator
  between two contiguous files.
* It _could_ be legitimate for there to be a 0-block file chunk as a
  separator between two contiguous empty spans.  The problem statement
  isn't exactly explicit about that.  My puzzle input doesn't have
  any, and assuming it doesn't enables a nice simplification later
  on^[Can you spot it?], so I'll gracefully accept it.

We can now convert that list of chunks into a full-blown list of
blocks, each decorated with their empty status or contained file id
respectively.

> expand :: Foldable f => f Chunk -> [Maybe FileId]
> expand chunks = foldMap expandChunk chunks where
>   expandChunk (Chunk width file) = replicate width file

In part 1, we compact by repeatedly moving the last non-empty block of
the disk to the first empty position.  Let's represent the disk as an
array.

> part1 :: [Maybe FileId] -> Array Int (Maybe FileId)
> part1 disk = runSTArray $ do
>   arr <- newListArray (0,length disk - 1) disk

We could implement by linearly scanning the array to identify the two
indices to exchange, but doing this at each iteration would result in
a quadratic algorithm, which we originally decided we didn't want to.

Instead we'll notice the following: after we swapped two candidate
blocks, there can never be an exchange involving a position to the
outside of the disk segment they enclose.  Why?  If there was, that
exchange would not have been valid in the first place, as either the
left empty space wouldn't be the leftmost empty space or the right
file block wouldn't be the rightmost file block, since there'd be a
more outside pair available afterwards.

We'll just keep track of the current candidate pair, and advance each
cursor in a single direction only until they cross.  After they do, we
know we're done, since all points left of the cursors are filled, all
points right of the cursors are empty, and they just went past each
other, so effectively we only have two segments left on the disk: the
left one filled with files and the right one empty.

Which yields a nice linear algorithm.^[Directly in terms of disk size,
and indirectly (by a factor of up to 9) in terms of input size.]

>   let go a b = when (a < b) $ do
>         cur <- readArray arr a
>         case cur of
>           Nothing -> do
>             cur' <- readArray arr b
>             case cur' of
>               Nothing -> go a (b-1)
>               Just v -> do
>                 writeArray arr a (Just v)
>                 writeArray arr b Nothing
>                 go (a+1) (b-1)
>           Just _ -> go (a+1) b

We launch it by initializing the cursors with the disk boundaries, and
we're good to go!

>   getBounds arr >>= uncurry go
>   pure arr

Input is validated using a simple checksum function.

> checksum :: Foldable f => f (Maybe FileId) -> Int
> checksum = sum . zipWith f [0..] . toList where
>   f _ Nothing = 0
>   f a (Just b) = a * b

Part 2 is where it gets more interesting.  Instead of block by block,
we're moving files as a whole.  So the question of where to move them
(leftmost) is not as easily answered as in part 1: after a move,
there's no saying there will never be a move with a target more to the
left: it's entirely possible we'll encounter a later file that's
smaller, and can fit in a broader set of cracks, so to speak.

When pressured to get the stars, an iterated linear scan works, as our
input is small.  But let's do things right, now.

> part2 :: [Chunk] -> FingerTree (Max Int) Chunk
> part2 chunks = 

We're representing the disk as a balanced tree, chunks as leaves,
annotated with the size of the largest empty chunk per subtree.

We can now process chunks.  There's no win to be had altering the
input list, we'll just process them one by one, and proudly do nothing
when we encounter an empty space.

>   let mbMoveFile acc (Chunk _ Nothing) = acc

As mentioned earlier, we can't do the dual cursor thing directly in
this case, as the target position will jitter.  We _can_ keep a cursor
in the source space, though.  We'll implement it as a zipper: our
accumulator will always be a view on the left subtree where things
happen, and the right subtree where all is stable forever.

We peek at the rightmost chunk of the active subtree and check whether
it's the file we're currently trying to move.

>       mbMoveFile (ft,rest) file = let ft' :> chunk = viewr ft in
>         if chunk /= file
>         then mbMoveFile (ft',chunk <| rest) file

If it isn't, we move the chunk to the stable forever subtree and
retry.

If it is, we split the (rest of the) active subtree on that first
big-enough space.  This is a logarithmic-time operation.

>         else let (l,sr) = split (>= Max (chunkWidth chunk)) ft' in

Now it's entirely possible there isn't a big enough chunk left.  In
that case, do nothing and move on, almost the same as previously.^[Not
_the same_.  We're not retrying.  In the first case, we don't find the
next file to match at the cursor position, but we can't move on, the
move has to be attempted for that file.  In the second case, the file
was found and the move isn't possible, but it was attempted, and we
can stop worrying about that file.]

>           case viewl sr of
>             EmptyL -> (ft',chunk <| rest)

If a big enough space is there, first split it from the attached
subtree, then insert our file and the rest of the free space into
position.

>             space :< r ->
>               let space' = Chunk (chunkWidth space - chunkWidth file) Nothing
>                   l' = l |> file
>                   r' = space' <| r
>                   rest' = chunk { chunkFile = Nothing } <| rest
>               in (l' <> r',rest')

This may be tricky to visualize.  Here's an alternative representation
of the whole iteration flow:

    (                                    ft         ) >< rest
    ( (             ft'                ) |> file    ) >< rest
      ( l           >< (        sr   ) ) |> file      >< rest
        l           >< ( space  <| r )   |> file      >< rest
      ( l |> file ) >< ( space' <| r )   >< ( space'' <| rest )
      (   l'      ) >< (        r'   )   >< (        rest'    )

Now the iteration step is implemented, we just need to wrap it all up:

* construct the tree (`fromList`)
* reverse the input list of chunks as a list of files to operate on
* conclude by merging active and passive subtrees back to a single one

>   in uncurry (<>) $
>      foldl' mbMoveFile (fromList chunks,empty) (reverse chunks) 

And that's all there is to it.  We're only lacking a bit of support
code to implement the largest subtree empty chunk logic,

> instance Measured (Max Int) Chunk where
>   measure (Chunk w Nothing) = Max w
>   measure         _         = mempty

…and a main function.

> main :: IO ()
> main = interact $
>   show .
>   (   checksum . part1 . expand
>   &&& checksum . expand . part2
>   ) .
>   parse

Implementing the thing properly is *so* satisfying!  See you tomorrow!
