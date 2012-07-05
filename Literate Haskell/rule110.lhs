The Challenge
==================================================

The one-dimensional simple cellular automata
[Rule 110](http://en.wikipedia.org/wiki/Rule_110)
is the only such cellular automata currently known to be turing-complete,
and many people say it is the simplest known turing-complete system.

Implement a program capable of outputting
an ascii-art representation of applying Rule 110 to some initial state.
How many iterations and what your initial state is is up to you!

You may chose to implement rule 124 instead if you like
(which is the same thing, albeit backwards).

Bonus points if your program can take
an arbitrary rule integer from 0-255 as input
and run that rule instead!

[/r/dailyprogrammer challenge #72 easy](http://www.reddit.com/r/dailyprogrammer/comments/w1e7x/742012_challenge_72_easy/)

My Response
==================================================

Time for some knot-tying!

Bits
--------------------------------------------------

Well, first, let's start off with a simple datatype to represent "bits".

> data Bit = I | O
> 
> fromChar :: Char -> Bit
> fromChar '1' = I
> fromChar '0' = O
> fromChar _ = error "Bits can only be 1 or 0"
>
> toChar :: Bit -> Char
> toChar I = '1'
> toChar O = '0'
>
> instance Show Bit where
>   show b = [toChar b]
>   showList bs s = map toChar bs ++ s

Cells and CellLoops
--------------------------------------------------

OK, now a type to represent a cell. Cells have two neighbors and a value.

> data Cell = Cell { cellPrev :: Cell, cellVal :: !Bit, cellNext :: Cell }

Computations involving a cell's bit value should be straightforward,
so I've made that field strict.
The neighbor fields, however, will need to be lazy
in order to tie the knot as we shall soon see.
Basically, I want this to be a circular doubly-linked list.
But we need to be able to have some notion of
when we have gone all the way around the "loop",
so we'll wrap up our cells in another data type to keep our bearings:

> data CellLoop = CellLoop { loopStart :: Cell, loopLength :: !Int }

A CellLoop chooses a definitive "starting point" cell,
and contains the "length" of the loop.

Creating a CellLoop from a list of Bits
--------------------------------------------------

Now, given a list of Bits, we want to be able to create a CellLoop.
We'll do that by tying the knot like so:

> fromList :: [Bit] -> CellLoop
> fromList [] = error "Can't create an empty CellLoop"
> fromList bs =
>   let (this, last) = fromList' bs last this
>   in CellLoop this (length bs)
> 
> fromList' :: [Bit] -> Cell -> Cell -> (Cell, Cell)
> fromList' [] prev first = (first, prev)
> fromList' (x:xs) prev first =
>   let this = Cell prev x next
>       (next, last) = fromList' xs this first
>   in (this, last)
> 
> fromString :: String -> CellLoop
> fromString = fromList . map fromChar

fromList' takes three inputs:

 * the list of bits
 * the "previous" cell of the completed loop
 * the "first" cell of the completed loop

It has two outputs: the "first" and "last" cells of the completed loop.

In the base case, you can see that it simply regurgitates its inputs.
In the interesting case, this and next are defined with mutual recursion,
and letrec magic ties them together.

Converting back
--------------------------------------------------

Converting back to a list of bits is much easier,
we just use the length that we stored as "fuel",
and when the fuel runs out, we stop.

> toList :: CellLoop -> [Bit]
> toList (CellLoop c i) = toList' c i
> 
> toList' :: Cell -> Int -> [Bit]
> toList' _ 0 = []
> toList' (Cell _ x next) i = x : toList' next (pred i)

Now, we actually want a CellLoop to display a little differently
than just a list of Bits, so we'll make a show instance accordingly:

> instance Show CellLoop where
>   show = map toChar' . toList
>     where
>       toChar' I = '*'
>       toChar' O = ' '

Evolution
--------------------------------------------------

Now for the final hurdle: evolution.
We'd like to write a function
`evolve :: Rule -> CellLoop -> CellLoop`.
In order to do so, we'll use both of the tricks we used previously:
tying the knot, and fuel.

> type Rule = Int
>
> evolve :: Rule -> CellLoop -> CellLoop
> evolve r (CellLoop c i) =
>   let (this, last') = evolve' r c i last' this
>   in (CellLoop this i)
>
> evolve' :: Rule -> Cell -> Int -> Cell -> Cell -> (Cell, Cell)
> evolve' _ _ 0 prev' first' = (first', prev')
> evolve' r c i prev' first' =
>   let newVal = evolveCellVal r c
>       this = Cell prev' newVal next'
>       (next', last') = evolve' r (cellNext c) (pred i) this first'
>   in (this, last')

> evolveCellVal :: Rule -> Cell -> Bit
> evolveCellVal r (Cell prev x next) =
>   lookupRule r (show [cellVal prev, x, cellVal next])
>
> -- currently ignores input rule and uses Rule 110
> lookupRule :: Rule -> String -> Bit
> lookupRule _ str = case str of
>   "111" -> O; "110" -> I; "101" -> I; "100" -> O
>   "011" -> I; "010" -> I; "001" -> I; "000" -> O

Since a Cell always knows about its neighbors,
the computation of the evolved cell value can be completely separate
from the code that traverses and reconstructs the CellLoop structure.

It should be straightforward,
given a technique to turn an integer into a list of bits,
to parameterize evolveCellVal (and by extension, evolve) on any rule.
This is left as an exercise to the reader.

Play time
--------------------------------------------------

Let's write a little helper to aid us in playing with what we've got:

> runRule :: Rule -> Int -> String -> IO ()
> runRule r i s = mapM_ print $ take i $ iterate (evolve r) $ fromString s

Let's play a little bit and see how it goes:

    [ghci]
    runRule 110 5 "100100100"
    runRule 110 10 "0000011111000000"
    runRule 110 10 "10000000101000000001"

You can play with this code yourself by downloading
[rule110.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/rule110.lhs).

