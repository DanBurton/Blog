#Hope for Haskell

Haskellers, I'd like to know your thoughts about something.

As an aspiring GSOC student,
I've been contemplating the current state of cabal-install and hackage.
These tools can be very convenient,
and have been an great source of joy and awesomeness to the Haskell community.
However, there are subtletries and pains to using these tools.
It can be hard for hobbyist Haskellers
to understand how to deal with a failed `cabal install foo`,
and it can be hard for "serious" Haskellers
to select an appropriate library for their pressing needs.

So I've been thinking.
The Haskell community could benefit from two things:

1. a simpler cabal-install that "just works"
or else tells you plainly and up front
that what you are trying to do won't work, and

2. an extended Hackage,
that gives you a better idea about the quality of a given library,
and guides you to good libraries.

Let's bundle these two concepts together
(a new command line tool built on top of cabal-install,
and a new website built on top of hackage)
and give them the code name "Hope".
You could think of Hope (partially) as being
a sort of crowd-sourced Haskell Platform.

The reason I suggest to bundle these tools is because
the command line tool can take advantage of
the "additional information" on the website
in order to make intelligent decisions.

**My ideas about Hope on the command line**

* phone home automatically
    * Report what OS, GHC version, etc, are being used
    * collect statistics, bugs
    * use this data to download versions of packages that are known to work on that setup
    * if compiled from source, upload the resulting binaries
* provide an "uninstall" option
    * keep track of installs
    * *don't* uninstall portions needed by other installed packages, print message but keep it simple
* provide an "upgrade" option
    * uninstall the old and then reinstall the new (?)
    * easy way to upgrade your GHC and reinstall all your libraries
* notify *upfront* if certain system libraries are needed (*.dll, *.so, alex, curl, etc)
    * this will require additional information, probably gathered from the website
* exlore other interesting possibilities
    * faster version of "cabal update" based on diffs, hashes
    * torrent package distributions
    * distribute pre-compiled binaries

**My ideas about Hope on the web**

* user-submitted ratings
* automated reports: does it pass hlint, compile without warnings, meet certain style guides?
* cascading ratings: you are only as good as your dependencies
* encourage a unified release cycle, rate activity of package
    * *don't* just grab the latest from Hackage until it has been proven to work correctly with things that depend on it.
* require a certain level of documentation to even show up on Hope
* rate levels of documentation, present well-documented libraries more prominently
* integrate Hayoo (Hoogle for Hackage)
* note assumptions about package (assumptions propagate to anyone depending on you)
    * cannot be installed alongside package X
    * must have certain libs or executables installed
    * unix only
    * assumptions/dependencies given in the .cabal file

Basically Hope on the web would be an enhanced Hackage
with a certain level of human intervention.
Hard to say how much is the "right amount",
but there should *at least* be
some design work and scavaging done manually
in order to present a sexy website,
perhaps showcasing high-quality libraries.
I'm also contemplating mandatory progress reports
from listed maintainers,
or something like that.

The aim of Hope is several-fold.
Primarily, it aims to just plain make Hackage and cabal-install better.
Note it does not aim to *replace* Hackage and cabal-install,
rather, to *use* and *build off of* them.
Another major goal is to encourage communication.
Rather than the one-way stream of communication that is currently the detault:
"upload to hackage -> download with cabal-install",
information and sharing automatically flows both ways:
"this build worked for me",
download statistics,
user ratings, etc.
Also, unified release cycles and mandatory progress reports could help
keep the dialogue flowing between upstream and downstream libraries,
as well as users.
If all goes according to plan,
then Hope will make Haskell an even more attractive language.
Hope eliminates complaints that Haskell is not as easy as pip/gem/npm,
and combats criticism of hit-or-miss library experiences.
Haskell should have Hope for a bright future.

But what do you think?
Would it be too much work?
Too many features?
Too little benefit?
Too elitist?
Name too cheesy?
This is almost certainly more work
than a single GSOC could produce,
but I think in the long run
Hope is the way to go.
Please comment, in order of preference,
on the reddit post, my google+ post, or the mailing lists.
