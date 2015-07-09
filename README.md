# URL Text Histogram

A toy two-page web service that generates a table of all words and
corresponding frequencies for a given URL.

## Building

Dependencies:

- `ghc` 7.8 or greater
- `cabal-install` 1.20 or greater
- `lynx`
- `postgreSQL` 9.4 or greater

Installing the dependencies on OSX is easy if you have homebrew:

```sh
> brew update
> brew install ghc cabal-install lynx postgres
```

Building is not quite as easy, but we can push through it _together_. Clone
this repo to your directory of choice, `cd` into said directory, ensure
Postgres is running, and then:

```sh
> bin/db create                      # create database and apply the schema
> cabal sandbox init                 # keep app packages out of global listing
> cabal install --only-dependencies  # install only what is needed to run
> cabal run 1337                     # or the port number of your choice
```

That's it! Now just visit `localhost:1337` to use the running application.

## Caveats

- The form only allows full URLs (including the host).
- Input and server are not particularly user-friendly.

## What's the Point?

There's actually two points:

1. I was asked to build this as part of a job application.
2. I've repeatedly heard that "_Haskell-framework-of-your-choice_ is just a
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
assisting with construction, I've managed to learn quite a bit about HTTP _from
the types alone_.

I've abstracted this concept into a currently unrefined talk, [Learning
through Libraries][4].

  [4]: https://github.com/Jonplussed/talks/tree/master/learning_through_libraries

#### The HTTP request-response pipeline is relatively simple.

These results really call into question the intentional referential opacity of
frameworks like Rails. The functional pipeline of languages like Haskell
ensures that the HTTP request and subsequent response can be traced
throughout the lifetime of the program, and without having to consult
entropied and unproven documentation.

#### The compromises required when adopting a framework are _heavy_.

Did you know that the HTTP spec doesn't describe how conflicting query
and POST body parameters should be resolved? This means the arbitrary decision
is left to each framework to decide— and for the programmer to discover via
ad-hoc experimentation. And this holds for many more arbitrary decisions.

Constructing a service from this lower level— but with the assistance of
types— means that the arbitrary decisions are yours and are discernible via
the aforementioned referential transparency.

#### The benefits of Haskell are more in the longer-term.

I often describe Haskell as "front-loading the programming effort", because the
types ensure that any possible error states are described and resolved. The
greater payoff exists in the longer-term, because once constructed, such
systems are demonstrably more robust.

These benefits are not realized when authoring throwaway applications, meaning
the programmer must suffer through most of the front-loaded effort without
seeing the greater payoff.

#### I would sans-doubt build another system in this manner.

The ability to have this finer-grained (yet robustly structured because of
types) control over the HTTP request/response lifetime would have proven
invaluable on any larger applications I've dealt with. And again, I cannot
overstate the value of referential transparency for any long-term software
system.

## I noticed you're not actually parsing the HTML

And why would I? You know what's really great at parsing HTML? A browser. And
what's really great at determining the textual content of a web page? A
text-only browser.

That's why instead I pass the validated URL to Lynx and process the streaming
results via [conduits][5] (which HOMG are awesome and you should totally use
them in any data processing applications!).

  [5]: http://hackage.haskell.org/package/conduit
