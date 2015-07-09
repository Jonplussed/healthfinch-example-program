# URL Text Histogram

A toy two-page web service that generates a table of all words and
corresponding frequencies for a given URL.

## What's the Point?

There's actually two points:

1. I was asked to build this as part of a job application.
2. I've repeatedly heard that "`[fill in a Haskell web framework]` is just a
   thin wrapper around [WAI][1]", and I wanted to know if it was true.

  [1]: http://hackage.haskell.org/package/wai

As for #2, that _really_ depends on what you mean by _thin_. If you don't want
to find yourself manually appending headers to an arbitrary HTTP response, I'd
recommend starting with [Scotty][2] for general HTTP request routing, parameter
parsing, and typical response generation.

  [2]: http://hackage.haskell.org/package/scotty

_That said..._

## Conclusions

The description of this problem as per the job application was that something
of this simplicity should only require a few hours development time.

This took me a few days.

The description would've held true had I spun this up in [Ruby on Rails][3] or
some similar "five-minute blog" web framework; for that purpose alone this
would've been _massive_ overkill. However, the fruits from this experiment have
been myriad. I've learned that:

  [3]: http://rubyonrails.org

### Types are _absolutely_ for humans first.

I cannot imagine attempting an operation at this level or lower without types
to ensure the correctness of and guide the architecture of this program. Beyond
assisting with construction, I've managed to learn quite about HTTP _from the
types alone_.

I've abstracted this philosophy into a currently unrefined talk, [Learning
through Libraries][4], which I hope to refine into something worth giving at
some beginner Haskell conference track.

  [4]: https://github.com/Jonplussed/talks/tree/master/learning_through_libraries

#### The HTTP request-response pipeline is relatively simple.

These results really call into question the intentional referential opacity of
frameworks like Rails. The functional pipeline of languages like Haskell
ensures that the HTTP request and subsequent response can be traced
throughout the life of the program without having to consult entropied
documentation— no meta-programming magic required!

#### The compromises required when adopting a framework are _heavy_.

Did you know that the HTTP spec doesn't describe how conflicting query
and POST body parameters should be resolved? This means the arbitrary decision
is left to framework to decide— and for the programmer to discover via ad-hoc
experimentation. And this is true for many more arbitrary decisions.

Constructing a service from this lower level— but with the assistance of
types— means that the arbitrary decisions are yours and are discernible via
the aforementioned referential transparency.

#### The benefits of Haskell are more in the longer-term.

I often describe Haskell as "front-loading the programming effort" because the
types ensure that any possible error states are described and resolved. The
greater payoff exists in the longer-term, because once constructed, such
systems are demonstrably more robust.

These benefits are not realized when authoring throwaway applications, meaning
the programmer must suffer through most of the front-loaded effort without
seeing the greater payoff.

#### I would absolutely build another system in this manner.

The ability to have this finer-grained (yet robustly structured because of
types) control over the HTTP request/response lifetime would have proven
invaluable on any larger application I've dealt with. And again, I cannot
overstate the value of referential transparency for any long-term software
system.

## You're not actually parsing the HTML

And why would I? You know what's really great at parsing HTML? A browser. And
what's really great at determining the textual content of a web page? A
text-only browser.

That's why I simply pass the validated URL to Lynx and process the streaming
results via [conduits][5] (which HOMG are awesome and you should totally use
them in any data processing applications!).

  [5]: http://hackage.haskell.org/package/conduit
