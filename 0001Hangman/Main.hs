module Main where

import           Hangman (hangmanGame)

main :: IO ()
main = hangmanGame "words.txt"
