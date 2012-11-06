Last time, we implemented a bowling game scorer
by using a Tardis. If you aren't yet familiar with
the Tardis's interface, then I recommend you check out
[the explanation on Hackage](http://hackage.haskell.org/packages/archive/tardis/0.3.0.0/doc/html/Control-Monad-Tardis.html).
(tl;dr it's a State monad with get and put,
except there are two streams of state,
one forwards and one backwards,
so there are four operations:
`(++) <$> [get, send] <*> ["Past", "Future"]`).

Today, we'll take a large step in the esoteric drection,
and implement a Seer by using a Tardis.

What is a Seer?
======================================================================

todo

Seer in terms of a Tardis
======================================================================

todo

Seer in terms of a Reader/Writer
======================================================================

todo

So why use a Tardis?
======================================================================

todo

