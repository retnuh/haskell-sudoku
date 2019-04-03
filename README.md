# haskell-sudoku

An implementation of a Sudoku solver in Haskell, similar/ported from my [Monadoku](https://github.com/retnuh/monadoku) project (which, sadly, fizzled just at the point of using monads to structure things - oh well).

Mostly as a way of learning me some Haskell. It's a non-trivial problem that
I'm familiar with so the focus is on getting the feel for the language, toolchain and libraries,
rather than wrestling with the problem itself.

I've started with the "good enough" version, that solves most (all?) Sudoku that don't require backtracking.
I may or may not implement a version that uses backtracking. Having done one in the clojure project,
and it being a pain in the arse, I'm disinclined. On the other hand, it may be an interesting exploration
of Haskell's laziness.... Hrmmm.

## Running, building, testing

This project uses [Stack](https://www.haskellstack.org/) as the build tool, so uses the usual
`stack build`, `stack test`, and `stack run` commands.

```{bash}
$ stack run
┌─────────────────────────────┬─────────────┬──────────────┬───────────┬──────────┬───────────────┬────────────────────┐
│           Solver            │   Puzzle    │ MessageQueue │ Complete? │ Correct? │ Messages Used │ Messages Remaining │
╞═════════════════════════════╪═════════════╪══════════════╪═══════════╪══════════╪═══════════════╪════════════════════╡
│ LSWSolver                   │ euler       │ set          │ True      │ True     │ 1018          │ 771                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ set          │ True      │ True     │ 1048          │ 640                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ set          │ True      │ True     │ 1054          │ 701                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ set          │ True      │ True     │ 1426          │ 458                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ set          │ True      │ True     │ 1791          │ 123                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ set          │ True      │ True     │ 1508          │ 460                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ LSWSolver                   │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ set          │ True      │ True     │ 1677          │ 203                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ list         │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ euler       │ dlist        │ True      │ True     │ 1938          │ 18                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ set          │ True      │ True     │ 1594          │ 298                │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ list         │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ easy        │ dlist        │ True      │ True     │ 1920          │ 21                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ set          │ True      │ True     │ 1892          │ 44                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ list         │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ mild        │ dlist        │ True      │ True     │ 1961          │ 19                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ set          │ True      │ True     │ 1885          │ 71                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ list         │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult1  │ dlist        │ True      │ True     │ 1996          │ 8                  │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ set          │ True      │ True     │ 1959          │ 25                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ list         │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ difficult25 │ dlist        │ True      │ True     │ 2007          │ 22                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ set          │ True      │ True     │ 1994          │ 36                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ list         │ True      │ True     │ 2061          │ 17                 │
├─────────────────────────────┼─────────────┼──────────────┼───────────┼──────────┼───────────────┼────────────────────┤
│ PartialApplicationLSWSolver │ fiendish    │ dlist        │ True      │ True     │ 2061          │ 17                 │
└─────────────────────────────┴─────────────┴──────────────┴───────────┴──────────┴───────────────┴────────────────────┘
1.75s user 0.28s system 100% cpu 2.032s total
```

## Thoughts

Certainly a useful learning experience. Some key points, in no particular order.

- Partial application is a very useful way of enclosing information to line up types, especially w/ state monad. It's basically a super convenient way of creating closures.
- Lenses are cool and interesting, but definately very hard for a beginner. In particular, the polymorpism stuff has some gotchas, I spent a lot of time fighting the compiler until I learned that you can't use them in let simple (untyped) let bindings, and they're hard to pass around between functions. They are handy for deeply nested state though, which is what the problem is all about.
- The error messages from GHC, while obscure, slowly start to make sense (at least part of the time :) )
- Debugging can still be quite challenging. I had to jump through some hoops to get readable trace output. I was having some problem where output from trace calls was all interleaved, even though, as far as I'm aware, I don't have any threading. In the end I ended up having to run things from ghci invoked with the following bit of shell magic (should work in bash and zsh): `stack ghci > >(tee /tmp/stdout.log) 2> >(tee /tmp/stderr.log >&2)`
- The Writer monad (transformer) is pretty cool. It's a nice way of separating out the generation of new messages from the updating of state. While working on this, one of the challenges of reading the Clojure code was separating out some of the funkiness for dealing with the two things at the same time.
- Code density between the Clojure and Haskell versions is pretty similar.
- My editor setup could use some work; I've been trying VSCode + [Haskell Language Server](https://github.com/alanz/vscode-hie-server) plugin. It's mostly okay except things get in a weird state occasionally where I'll edit a file, save, and it will nuke the change I just make until I go into the shell and do a `killall hie-wrapper`.
- The `MessageQueue` typeclass, while ultimately reasonably clean, took a long time and I spent quite a bit of time bouncing off the compiler. Still have a fair bit to learn in terms of when to use contraints for types, etc.
- Using the `PartialApplicationLSWSolver` I was able to remove the use of partial functions and possible errors present in the basic `LSWSolver`, but you can see that, with the `Set` based `MessageQueue`, that it suffers - this is due to the fact that you can't really compare functions for equality. I'm still interested in taking a look at type/data families to see if I can get the best of both worlds.

## Future stuff

I've done the first of these, and may try second; the rest aren't too likely (at least at the moment).

- ~~make a `MessageQueue` typeclass to see if different ways of gathering the messages can reduce the message count. It's essentially just a `Monoid` and I think a sorted list should see some improvements if the `IsValue` messages get delivered before other messages.~~
- Look at type families to deal with the issue of some of the partial functions. I'm interested in using Haskell to make clearly wrong things inexpressible rather than just something to avoid. **UPDATE** I got this working so far using just partial application of functions; see comment in thoughts above.
- Look at an implementation using the [Conduit](https://github.com/snoyberg/conduit) library. This would be a pretty different implementation paradigm (maybe) so could be interesting. Also looks like a very interesting library. **UPDATE** Having looked at this a little more it's not clear that it would actually be useful for an implementation - a bit of square peg/round hole going on.
- Also look at doing an implementation using [FRP](https://wiki.haskell.org/Functional_Reactive_Programming) using maybe [Reactive Banana](https://wiki.haskell.org/Reactive-banana) or [Reflex](https://github.com/reflex-frp/reflex).
- Possibly implement the addtional "contstrained by box" rule I've discovered but never implemented in the Clojure version. I don't know if this would solve the "hardest" puzzle or not, but it should be much easier to implement than a full backtracking/search based soultion.
- Implement a backtracking/search solution. Perhaps from scratch, rather than sitting on top of exisiting solution. Dunno.

## Feedback welcome

I realize that this is quite unlikely to be looked at by anybody, but if anybody does look at it and has any questions, comments, feedback, suggestions or anything else, I'd love to hear it. Feel free to open a github issue or whatever.
