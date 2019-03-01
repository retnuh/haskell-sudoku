# haskell-sudoku

An implementation of a Sudoku solver in Haskell, similar/ported from my [Monadoku](https://github.com/retnuh/monadoku) project.

Mostly as a way of learning me some Haskell. It's a non-trivial problem that
I'm familiar with so the focus is on getting the feel for the language, toolchain and libraries,
rather than wrestling with the problem itself.

I'll start with the "good enough" version, that solves most (all?) Sudoku that don't require backtracking.
I may or may not implement a version that uses backtracking. Having done one in the clojure project,
and it being a pain in the ass, I'm disinclined. On the other hand, it may be an interesting exploration
of Haskell's laziness.... Hrmmm.
