  One of the Facebook Hacker Cup (2012) challenges in Round 1
was, given a list's length "n", and a series of "choices" taken by
a typical merge sort algorithm, discover the original list.
You additionally make the assumption that the original list
was a permutation of [1 .. n].

> {-# LANGUAGE BangPatterns #-}

> import Control.Applicative
> import Control.Monad
> import Control.Monad.Writer.Lazy
> import Control.Monad.State.Lazy
> import System.IO
> import Text.Printf (printf)

  I started out by playing around with the idea of a "monadic"
merge sort, a la sortBy. The difference is that the comparator
you pass in is allowed to perform an arbitrary monadic effect.
I gave this concept a type synonym, for clarity. An MComparator
is a function that takes in two things of the same type,
and then produces a monadic action that yields an Ordering.

> type MComparator m a = a -> a -> m Ordering

  Writing sortByM was trivial. In order to get the correct answer,
I had to mimick the merge sort pseudocode provided by Facebook,
which was pretty standard. The only thing that makes this Haskell
code strange is that it is necessarily monadic, since it permits
the comparator to execute arbitrary monadic effects.

> sortByM :: (Monad m, Functor m) => MComparator m a -> [a] -> m [a]
> sortByM cmp []  = return []
> sortByM cmp [x] = return [x]
> sortByM cmp xs = do
>   let (ys, zs) = partition xs
>   ys' <- sortByM cmp ys
>   zs' <- sortByM cmp zs
>   merge ys' zs'
>   where merge [] bs = return bs
>         merge as [] = return as
>         merge (a:as) (b:bs) = do
>           comparison <- cmp a b
>           case comparison of
>             LT -> (a:) <$> merge as (b:bs)
>             _  -> (b:) <$> merge (a:as) bs
>         partition xs = splitAt (length xs `quot` 2) xs

  Now then, we have a lovely higher-order function. Let's play!
We need some MComparators to feed to this baby, and I immediately
had two ideas.

1. Mimick the Facebook code, and record the choices made by
   a regular comparison sort. This simply *screams* Writer monad.

> spewCmp :: (a -> a -> Ordering) -> MComparator (Writer [Choice]) a
> spewCmp cmp x y = case cmp x y of
>   LT -> tell [LeftFirst] >> return LT
>   x -> tell [RightFirst] >> return x

2. Completely ignore the inputs, and perform the "sort"
   based on a given list of choices.

> givenCmp :: MComparator (State [Choice]) a
> givenCmp x y = do
>   (c:cs) <- get
>   put cs
>   return $ toOrdering c

  The State monad makes this really elegant.
Notice how, with this approach, I don't have to tell the sort algorithm
how many choices to take from the list when it enters a sub-branch.
Since it all runs in the same state monad, it Just Works.

  givenCmp isn't the safest function (what happens when you run out
of choices?) but hey, I'm just playing around. As a side note,
I made a simple data structure to represent a "choice"
made by the merge sort algorithm, which you may have noticed
in the code above. Nothing too special.

> data Choice = LeftFirst | RightFirst deriving (Eq)

> instance Show Choice where
>   show c = [fromChoice c]

> fromChoice LeftFirst = '1'
> fromChoice RightFirst = '2'

> toChoice '1' = LeftFirst
> toChoice '2' = RightFirst

> toOrdering LeftFirst = LT
> toOrdering RightFirst = GT

  Still not too serious yet, I figured I'd try asserting things
about how these two different monadic sorts behave.

  If you record the steps of a regular compare sort,
and then perform a "sort" immitating those steps,
you will get the same result both times.

  Also, you will consume exactly the same number of choices,
so the end state of the latter computation will be
an empty list of choices.

> prop_sorty :: [Int] -> Bool
> prop_sorty xs = null s && xs' == xs''
>   where (xs', log) = runWriter $ sortByM (spewCmp compare) xs
>         (xs'', s)  = flip runState log $ sortByM givenCmp xs

  Well this was all good fun, but it's about time I started
actually solving the problem. At first, I tried to read the
specified list of choices backwards, and reconstruct the original
list by "unsorting" the sorted list. This lead to much pain
and agony.

  Then, I had an epiphany. I already had all the tools I needed
to solve this problem in a very elegant way. Let's define some
helpers to illustrate more clearly: these are just convenience
wrappers around the cruft of running Writer and State.

> -- given a list of merge choices, runs those mergesort steps on a list
> runChoices :: [Choice] -> [a] -> [a]
> runChoices cs = flip evalState cs . sortByM givenCmp

> -- sorts a list the usual way, making note of the choices made at each step
> recordChoices :: Ord a => [a] -> [Choice]
> recordChoices = execWriter . sortByM (spewCmp compare)

  Note that runChoices doesn't even require type "a" to have an Ord instance!
Also note that we discard the uninteresting parts of running State
and Writer by using evalState (keep result, discard state)
and execWriter (keep log, discard result) respectively.

  Now consider one particular example given by Facebook:
n = 4, choices = 12212, solution = [2,4,3,1]. Let's play around
in ghci a little bit. Recall that in ghci,
"it" refers to the result of the previous query.

ghci> recordChoices [2,4,3,1]
[1,2,2,1,2]
ghci> runChoices it [1,2,3,4]
[4,1,3,2]

  There are a couple things to notice about this.
First of all, the mapping. Compare [2,4,3,1] to [1,2,3,4].
2 maps to 1, 4 maps to 2, 3 maps to 3, and 1 maps to 4.
Now compare [1,2,3,4] with [4,1,3,2].
1 maps to 4, 2 maps to 1, 3 maps to 3, 4 maps to 2.
Notice anything? It's the *same mapping*.
Given the mapping from [1,2,3,4] to [4,1,3,2],
we should be able to reverse it, and then apply the
reverse map on [1,2,3,4] in order to recover the original!

  But I actually solved it slightly differently.
You see, [4,1,3,2] can be transformed back into [1,2,3,4]
by sorting it, right? We arrived at [4,1,3,2] by applying
the choices of sorting [2,4,3,1] onto [1,2,3,4].
So moving from [1,2,3,4] to [2,4,3,1] *represents*
moving from the original list to [1,2,3,4],
since we made the *same choices* in both cases. So now
what about moving back?

  If we record the choices made moving back from
[4,1,3,2] to [1,2,3,4] (which can be done by sorting!)
then the *same choices* can be used to "move back"
from [1,2,3,4] to the original list!

> solvePerm :: Int -> [Choice] -> [Int]
> solvePerm n cs = runChoices (recordChoices (runChoices cs xs)) xs
>   where xs = [1 .. n]

  Of course, we *must* write a quickCheck property to make it legit.
If you record the steps of a regular compare sort,
then you can use solvePerm to get the "correct answer".
To prove it is correct, you record the choices of compare sort on *that*
and observe that they are the same.

> prop_solved :: [Int] -> Bool
> prop_solved xs = cs == cs'
>   where cs  = recordChoices xs
>         cs' = recordChoices $ solvePerm (length xs) cs

  The particular way I wrote this skirts around an issue
with using quickCheck: we should actually be generating [Int]
such that it is a permutation of [1 .. n]. I avoid caring
about this detail by taking a list of choices to be a sort of
"signature" for a given permutation. Thus, any random list
"represents" a given permutation, because it produces
a particular list of choices.

  Well, there you have it. The rest of the problem is just boilerplate,
and if any of you are using Haskell for the Facebook Hacker Cup,
then feel free to steal this main method boilerplate, which I have used
with only light modification on pretty much every problem:

> inFileName = "recovery-in.txt"
> outFileName = "recovery-out.txt"

> readInt :: String -> Int
> readInt = read

> zipSelf :: [a] -> [(a,a)]
> zipSelf (x:y:xs) = (x,y) : zipSelf xs
> zipSelf _ = []

> main = do
>   contents <- words <$> readFile inFileName
>   outFile <- openFile outFileName WriteMode
>   let n = readInt (head contents)
>       inputs = zipSelf $ tail contents
>   forM_ ([1 .. n] `zip` inputs) $ \(c, (num, choices)) -> do
>     let ans = checksum . solvePerm (readInt num) . map toChoice $ choices
>     hPutStrLn outFile $ printf "Case #%d: %d" c ans
>   hClose outFile

  Minor detail, the checksum, as specified by Facebook for this problem.
Can be written more succinctly with foldl'.

> checksum :: [Int] -> Int
> checksum = go 1
>   where go !res [] = res
>         go !res (x:xs) = go ((31 * res + x) `rem` 1000003) xs

