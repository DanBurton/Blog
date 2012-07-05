In response to
[Daily Programmer Challenge #71 intermediate](www.reddit.com/r/dailyprogrammer/comments/vx3db/722012_challenge_71_intermediate/).

You've probably seen the classic Haskell one-liner:

    [haskell]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

Let's generalize it to work with this problem.
Since I chose to use Integers everywhere,
I'll need lots of genericFoo from Data.List.

> import Data.List

Now first let's generalize `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`.
The zipping function, instead of taking 2 inputs, will take K inputs.
Then, instead of giving it 2 lists, we will give it K lists.
It will be slightly *less* general,
in that all K inputs will have the same type `a`,
rather than differing types `a` and `b`.

Let's use a list of length K to encode this sort of input.
Therefore, `(a -> b -> c)` becomes `(List K a -> c)`,
and `[a] -> [b] -> [c]` becomes `List K [a] -> [c]`.
However, I won't actually bother encoding
how long the list is into the type system,
so it'll just be `[a] -> c` and `[[a]] -> [c]` respectively.

I will implement it by taking in some function `f`,
and some list `xss`. The first entry of the resultant list
will be the result of applying f to all the first entries of `xss`,
and so forth:

    [haskell]
    listZip :: ([a] -> b) -> [[a]] -> [b]
    listZip _ []  = []
    listZip f xss
      | null (head xss) = []
      | otherwise = f (map head xss) : listZip f (map tail xss)

Actually, there's an easier way to implement it, using Data.List:

> listZip :: ([a] -> b) -> [[a]] -> [b]
> listZip f = map f . transpose

Now, I must generalize `(+)` to work on lists.
The obvious generalization is `sum`.
I'm making one additional tweak, which is to calculate the sum modulo M.

> sumMod :: Integer -> [Integer] -> Integer
> sumMod m = foldl' (\x y -> (x + y) `rem` m) 0

The generalization of `tail` is already written for me:
it is `tails` from `Data.List`.
Now to generalize the rest of `fibs`.
I'll parameterize it by M (the modulus)
and K (as described earlier), as follows:

> fibSeq :: Integer -> Integer -> [Integer]
> fibSeq m k = fibs
>  where
>   fibs = genericReplicate (pred k) 0 ++
>          1 :
>          (listZip (sumMod m) $ genericTake k $ tails fibs)

From here the desired function `f` as specified in today's problem is simple:

> fibSeqAt :: Integer -> Integer -> Integer -> Integer
> fibSeqAt m k n = fibSeq m k `genericIndex` n

This code therefore works by lazily constructing
the Kth Fibonacci sequence (modulo M),
and then inspecting its Nth element.
Modular arithmetic assures that aggressive truncation
still preserves the same truncated sum.

Testing:

    [ghci]
    mapM_ (print . take 20 . fibSeq 100) [1 .. 5]
    fibSeqAt (10^8) 100 10000

This solution is still too slow, however, to reasonably compute
`fibSeqAt (10^8) (3^13) (5^10)`.

You can play with this code yourself by downloading
[fib.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/fib.lhs).