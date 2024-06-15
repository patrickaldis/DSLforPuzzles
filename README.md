<p align="center">
  <img src="doc/nurikabe.svg" />
</p>

# A DSL for Puzzles
> This project was produced for the dissertation component of my Computer Science masters at University of Warwick. The accompanying thesis can be found  [here](doc/report.pdf).

The aim of the project is to create an expressive framework for stating and solving simple grid puzzles. It aims to provide:

- [x] A Haskell **DSL** for stating puzzles
- [x] A **solver** for producing solutions
- [ ] A **file format** to state puzzles in plain text
- [ ] A **front end** to enter, inspect and solve puzzles

The project is packaged using IOHK's `haskell.nix` infrastructure, and uses both the `sbv` library and microsoft's `z3` SMT solver under the hood.

The goal of the project is to provide a way to declaratively describe the rules of a puzzle using something close to predicate logic.

## Current State of the Project (June 2024)
- [ ] Restructure the language as a *Deep Embedding* rather than a *Shallow Embedding*. This will allow for much easier parsing when reading from a textual format, and greater control over the language. For further reference on shallow vs deep embedding see [here](https://alessandrovermeulen.me/2013/07/13/the-difference-between-shallow-and-deep-embedding/)
- [ ] Implementing expressing of connectivity between two cells. In my initial report, I identified that this would greatly increase expressiveness (and allow a lot of 'standard' puzzles to be described).

# Building
Currently, building the project runs the test suite. This contains unit tests for checking that the language behaves as desired, and other tests to check that specific puzzles can be solved.

## Nix
Ensure that [`nix`](https://nixos.org/) is installed, then run:

```
nix run
```

## Cabal
Ensure that `ghc` 9.2.7 is installed, with a recent version of `cabal` (this can all be done via the [`ghcup`](https://www.haskell.org/ghcup/) utility), then run:
```
cabal run
```

# Aknowledgements
I would like to give thanks to:
- [`IOHK`](https://iohk.io/about/) &mdash; For their robust `haskell.nix` infrastructure
- [`sbv`](https://hackage.haskell.org/package/sbv-10.2) &mdash; For their SMT solving framework
- My supervisor *Alex Dixon* for all his help
