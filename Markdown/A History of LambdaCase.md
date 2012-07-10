On December 16, 2005,
Haskell Prime trac ticket #41 was born,
with the humble title
[add LambdaCase](http://hackage.haskell.org/trac/haskell-prime/ticket/41).
The description field contained a pointer
to the
[LambdaCase](http://hackage.haskell.org/trac/haskell-prime/wiki/LambdaCase)
wiki article,
whose contents are also quite humble:

**case statements as first order**

a simple desugaring

    case of
       arms

gets desugared to

    \x -> case x of
        arms

That's right. Six and a half years ago,
Isaac Jones was thinking about how Haskell would be better
if it had LambdaCase.
Some of you readers might have been right there with him
six years ago -- or more -- wishing for this simple
extension to Haskell syntax.

Well the truth is, it almost certainly goes back much farther than that.
Trac history records that Haskell Prime tickets 13 through 74
were all created that very same day by ijones;
presumably they were copied over from some pre-existing system.
As someone who only started learning Haskell in late 2010,
anything that predates GHC 6.12 is ancient history for me,
so you'll have to ask one of the old sages for details
about the true origins of the idea of LambdaCase.

Fast forward to October 2, 2010, about two months before
I started reading Learn You a Haskell and falling in love
with a new language. batterseapower opened GHC trac ticket #4359 entitled
[Implement lambda-case/lambda-if](http://hackage.haskell.org/trac/ghc/ticket/4359).
He provided a patch implementing behavior similar to that
described in Haskell Prime trac ticket #41,
bundled with a similar feature for if-then-else syntax.

    Prelude> (if then "Haskell" else "Cafe") False
    "Cafe"
    Prelude> (case of 1 -> "One"; _ -> "Not-one") 1
    "One"

Discussion ensued. Would merely providing `mcase` be a better option?
Should this feature serve as "a standard lambda that can discriminate patterns
on the value being bound"? Simon Marlow impulsively supported lambda-case
and lambda-if at first, but then revoked his support, stating that:

> the downside is that we have to adjust our mental parsers/typecheckers
to recognise if then and case of as a lambda,
and I'm not sure the gain in brevity is worth the loss of readability.

Simon Peyton-Jones joined with Marlow
in scepticism of the initial proposed syntax, stating that:

> My own gut feel is that it's a lot better to have
an initial symbol to say "here comes a function".
So I'm keener on the multi-clause lambda idea,
if someone can devise a syntax that works.

The Simons put their heads together and suggested the following syntax:

    \case { blah } ==> \x -> case x of { blah }

Peyton-Jones suggested `\of` as a potential alternative to `\case`,
which would avoid creating a new "layout herald", given `of`
is already a layout herald. The `\of` idea seems to be popular
in a more recent discussion; we'll get to that later.

All the while, discussions on mailing lists were taking place.
Max Bolingbroke
[notified Haskell Cafe](http://www.mail-archive.com/haskell-cafe@haskell.org/msg82264.html)
of the ticket,
resulting in an explosion of ideas and discussion.
Back on trac, it was suggested that LambdaCase could support
multiple arguments:

    \case { (Just x) (Just y) -> x + y; _ _ -> 1 } 
      ==>
    \a b -> case (a, b) of { (Just x, Just y) -> x +y; (_, _) -> 1 }

Now, I believe there were two grave mistakes
that caused this ticket to be derailed.
First, it introduced a patch that implemented two separate features:
lambda-case and lambda-if.
Second, as it was discussed, the scope of it never seemed to stop growing.
People kept dreaming up new features that could be added into the original idea,
causing it to swell in complexity, making it difficult to pinpoint
what exactly should be implemented and whether it was a good idea at all.

Fast-forward again to April 2011. Following a few recent touches
to the ticket, Mikhail Vorozhtov introduced anoter patch,
implementing single-argument `\case` syntax.
The Simons again chimed in, this time less helpful than the last,
they both led conversation to tangential topics.
Marlow expressed interest in multi-argument `\case`, while
Peyton-Jones suggested simply using `\` instead of `\case`,
and therefore making `\` a layout herald.
The `\` idea received mostly negative feedback,
given that it would cause backwards incompatibilities in the way `\` works.

Nevertheless, Peyton-Jones made an important comment
in the midst of this portion of the discussion:

> ... every part of GHC from the parser onwards already implements lambda-case!
... All that lambda-case does is provide concrete syntax
to let you get at the abstract syntax that is already there.
... So I think lambda-case is a fine candidate
to fill the syntactic gap for now,
leaving something more ambitious for the future.

"Something more ambitious" referred to
a compositional approach to pattern matching,
and I couldn't agree more: that we need to fill the gap with lambda-case now,
and leave the ambitious, compositional solution for the future.

On May 12, 2011, Simon Marlow gave a deep stamp of approval
to the newly-submitted patch when he said:

> Patch looks great, thanks Mikhail!
We just need a test, and we can put it into 7.2.1.

Despite this, the milestone got pushed back from 7.2.1 to 7.4.1.
At this point, despite the negative feedback,
several people seemed to be pushing for the simpler `\` syntax.
SPJ suggested that to avoid conflicts, the feature
could be made available only with explicit layout
(using curly braces and semicolons).
However, Simon Marlow expressed dislike for explicit layout only,
and concluded:

> Perhaps the only way to do this
is to have a new keyword.

A few more suggestions floated about,
and the milestone was punted from 7.4.1 to 7.6.1.

Fast forward to May 2012.
SPJ downgraded the priority
"until we can find a better solution".
Mikhail, ever the champion of the cause,
wisely suggested splitting multi-arg `\case` into a separate ticket.
SPJ noted that "Simon and I are deeply under water
with other aspects of GHC,
which makes it hard to devote attention to new features."
Simon Marlow requested "a summary of the previous proposals
and their pros and cons, so that we don't have to rediscover all this."
(Note that this document is not geared towards achieving that goal.
Rather, my goal with this document is to merely expose
the long and epic journey of LambdaCase to the public eye.)
Marlow also suggested:

> Another alternative is to introduce a new keyword
for anonymous multi-clause multi-argument functions, e.g. `fun`

Fast forward again to July 5, 2012. Mikhail created a wiki page,
[LambdasVsPatternMatching](http://hackage.haskell.org/trac/ghc/wiki/LambdasVsPatternMatching),
essentially to fulfill Marlow's request of reviewing the pros and cons
of each of the major suggestions. Notably, Mikhail concludes from
his personal experience that multi-arg `\case` was entirely unnecessary;
single-arg `\case` was entirely sufficient, and in those few rare circumstances
where multiple matches were desirable, `curry $ \case ...` was sufficient.

Mikhail provided new patches, and started
[a thread](http://haskell.1045720.n5.nabble.com/Call-to-arms-lambda-case-is-stuck-and-needs-your-help-td5714299.html)
on the GHC Users mailing list.
The usage of parens were discussed,
and Simon's `\of` idea came to light again,
gathering strong support from various people.
A few (including myself) find this particular idea acceptable,
though "a little weird", and Mikhail stated:

> Do you think that adding `"\"` + `"case"` as a layout herald would
complicate the language spec and/or confuse users? Because it certainly
does not complicate the implementation (there is a patch for `\case`
already). IMO `\case` is more descriptive, "of" is just a preposition
after all.

Additional details regarding layout rules and "comma sugar"
are discussed in that thread.

Fast forward to today. The discussion is ongoing.
*You* have the opportunity to let your voice be heard.
Chime in on the mailing list. Code review the current proposed patch.
But promise me one thing.
Do you want LambdaCase, in *any* shape or form, to make it into GHC 7.6.1?
Then make yourself heard. Make it happen.
We're in the process of writing the latest chapter in LambdaCase history.
Let's *not* punt it to the next GHC release.
Let's make *this* chapter the one where it actually gets released.
And let's not stop there. It's been over six years. It's time for
dreams of Haskell Prime to start coming true. What will it take
to get this feature into the Haskell language itself?
Let's keep moving forward, and see that happen for ourselves.
