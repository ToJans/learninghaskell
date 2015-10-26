# Learning Haskell

Some random exercises I perform in order to get a better grasp on Haskell.

Goals:
- readable code
- type-driven development
- real-world cases, not some abstract mumbo-jumbo

## Running it

Just do `runhaskell x` to run an exercise in the main folder; for example:

```
E:\Dev\haskell\learning>runhaskell "0003 Sudoku.hs"
Sudoku solver by @ToJans

|-----------------------|
| 4 . 3 | 1 . . | 9 . 8 |
| . 2 9 | . . . | 4 5 . |
| 1 7 . | 8 4 9 | . . . |
|-----------------------|
| . 4 . | . 2 . | . . . |
| . 9 . | 4 . 7 | . 3 . |
| . . . | . 3 . | . 8 . |
|-----------------------|
| . . . | 5 9 8 | . 4 7 |
| . 5 7 | . . . | 8 6 . |
| 2 . 4 | . . 3 | 5 . 9 |
|-----------------------|

Finding solution
BoardSolved
|-----------------------|
| 4 6 3 | 1 5 2 | 9 7 8 |
| 8 2 9 | 3 7 6 | 4 5 1 |
| 1 7 5 | 8 4 9 | 3 2 6 |
|-----------------------|
| 3 4 8 | 6 2 1 | 7 9 5 |
| 5 9 6 | 4 8 7 | 1 3 2 |
| 7 1 2 | 9 3 5 | 6 8 4 |
|-----------------------|
| 6 3 1 | 5 9 8 | 2 4 7 |
| 9 5 7 | 2 1 4 | 8 6 3 |
| 2 8 4 | 7 6 3 | 5 1 9 |
|-----------------------|


E:\Dev\haskell\learning>
```

To run exercises in the sub folders, you need to install `cabal`.
Then you just go into the subfolder and type `cabal run` or `cabal repl`.
