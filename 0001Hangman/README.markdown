# Hangman game

This is an exercise to learn Haskell, and to see how `haddock` and `doctests` work.

In real life it probably wouldn't make sense to expose every single function in
the `Hangman` module, but for the purpose of learning how to write documentation,
I've done it anyway.

This should also be a good example about how literate docs need to be to be
usable by Haskell noobs like me.

You can find the generated docs [here](http://users.telenet.be/bull/learninghaskell/0001Hangman/Hangman.html).

## Installation

Make sure you have `git`, `Haskell` and `cabal` installed.

```
git clone https://github.com/ToJans/learninghaskell.git
cd learninghaskell/0001Hangman
cabal sandbox init
cabal install --dependencies-only
```
## Running the game

```
cabal run
```

## Generating docs

```
cabal haddock --executable
```

You can find the docs in the `dist/doc/html/Hangman/Hangman` folder.

## Running the doctests

This will run all the examples specified in the docs and verify
it's outcome with that in the documentation.

```
cabal test
```
