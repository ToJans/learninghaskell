-- | Hangman game by <https://twitter.com/tojans @ToJans>
--
-- Allows you to guess random words letter by letter.
-- If you have to many wrong attempts for one word, the man hangs, and the game
-- is over.
--
-- This is a learning experiment; more info at <https://github.com/ToJans/learninghaskell/tree/master/0001Hangman github>
--
-- You can find the generated docs over <http://users.telenet.be/bull/learninghaskell/0001Hangman/Hangman.html here>.

module Hangman where

import           Control.Monad (when)
import           Data.Char     (toLower)
import           Data.List     (transpose)
import           System.Random (randomRIO)

-- | The state of the word to guess
data WordState = WordState
    { _wordToGuess :: String -- ^ The word to guess
    , attempts     :: String -- ^ The letters attempted
    }

-- | The outcome of an attempt
data AttemptOutcome = GuessAnotherLetter   -- ^ Ready for another guess
                    | WordGuessed          -- ^ All the letters in the word were guessed
                    | TooManyWrongAttempts -- ^ Too many wrong attempts
                    deriving (Eq,Show)

-- | The hangman ascii images for every wrong guess
--
-- >>> putStrLn . unlines $ hangmanImages !! 6
-- _O_
--  |
-- / \
-- <BLANKLINE>
hangmanImages :: [[String]]
hangmanImages =
    transpose
    [ [ "   ", " O ", " O ", " O ", " O " , "_O " , "_O_"  ]
    , [ "   ", "   ", " | ", " | ", " | " , " | " , " | "  ]
    , [ "   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\" ]
    ]

-- | The hangman from the current phase hanging on a pole
--
-- >>> putStrLn . unlines $ fullHangmanImage  6
-- =========
-- |    |
-- |   _O_
-- |    |
-- |   / \
-- <BLANKLINE>
fullHangmanImage :: Int -> [String]
fullHangmanImage index =
    "=========" :
    "|    |" :
    map ("|   " ++) img
    where img = hangmanImages !! index

-- | The max number of wrong attempts allowed.
--
-- >>> maxWrongAttempts
-- 6
maxWrongAttempts :: Int
maxWrongAttempts = length hangmanImages - 1

-- | The current number of wrong attempts in a `WordState`
--
-- >>> numberOfWrongAttempts $ WordState "cat" ""
-- 0
-- >>> numberOfWrongAttempts $ WordState "cat" "a"
-- 0
-- >>> numberOfWrongAttempts $ WordState "cat" "ab"
-- 1
-- >>> numberOfWrongAttempts $ WordState "cat" "abcdef"
-- 4
numberOfWrongAttempts :: WordState -> Int
numberOfWrongAttempts (WordState word' attempts') =
    length $ filter charNotInWord attempts'
    where charNotInWord c = c `notElem` word'

-- | What is the outcome of the last attempt?
--
-- >>> lastAttemptOutcome $ WordState "cat" ""
-- GuessAnotherLetter
-- >>> lastAttemptOutcome $ WordState "cat" "abcd"
-- GuessAnotherLetter
-- >>> lastAttemptOutcome $ WordState "cat" "abcdefgh"
-- TooManyWrongAttempts
-- >>> lastAttemptOutcome $ WordState "cat" "abcdeft"
-- WordGuessed
lastAttemptOutcome :: WordState -> AttemptOutcome
lastAttemptOutcome wordState@(WordState word' attempts')
    | isGuessed   = WordGuessed
    | isLastGuess = TooManyWrongAttempts
    | otherwise   = GuessAnotherLetter
    where
        isGuessed = all isCharInGuesses word'
        isCharInGuesses x = x `elem` attempts'
        isLastGuess = numberOfWrongAttempts wordState >= maxWrongAttempts

-- | Get a `char` from `stdin`.
-- For one reason or another `Prelude.getChar` also appends a carriage return,
-- so I implemented my own and made sure empty input is refused.
--
-- > >>> getChar
-- > <<< a
-- > 'a'
getAChar :: IO Char
getAChar = do
    line <- getLine
    case line of
        [] -> getAChar
        (c:_) -> return c

-- | Gets an attempt that letter that's not in the previous attempts for this word.
--
-- > >>> getAValidAttempt $ WordState "cat" "abcd"
-- > <<< e
-- > 'e'
-- > >>> getAValidAttempt $ WordState "cat" "abcde"
-- > Next char to guess
-- > <<< d
-- > Character already used in attempts.
-- > Next char to guess
-- > <<< f
-- 'f'
getAValidAttempt :: WordState -> IO Char
getAValidAttempt wordState = do
    putStrLn "Next char to guess"
    c <- getAChar
    if c `elem` attempts wordState
    then do
        putStrLn "Character already used in attempts."
        getAValidAttempt wordState
    else
        return c

-- | Display the current state of the word.
--
-- >>> printWordState  $ WordState "cat" ""
-- =========
-- |    |
-- |
-- |
-- |
-- Word to guess: ___
-- <BLANKLINE>
-- Guesses:
-- <BLANKLINE>
-- >>> printWordState  $ WordState "cat" "abcd"
-- =========
-- |    |
-- |    O
-- |    |
-- |
-- Word to guess: ca_
-- <BLANKLINE>
-- Guesses: abcd
-- <BLANKLINE>
-- >>> printWordState  $ WordState "cat" "abcdeft"
-- =========
-- |    |
-- |    O
-- |    |
-- |   / \
-- CONGRATULATIONS!
-- You correctly guessed the word cat
--  in 7 tries
-- <BLANKLINE>
-- >>> printWordState  $ WordState "cat" "abcdefgh"
-- =========
-- |    |
-- |   _O_
-- |    |
-- |   / \
-- YOU FAILED!
-- You failed to guess the word cat
-- <BLANKLINE>
printWordState :: WordState -> IO ()
printWordState wordState@(WordState word' attempts')  =
    putStrLn $ unlines $ fullHangmanImage' ++ case lastAttemptOutcome wordState of
        GuessAnotherLetter ->
            [ "Word to guess: " ++ wordWithGuesses
            , ""
            , "Guesses: " ++ attempts'
            ]
        WordGuessed ->
            [ "CONGRATULATIONS!"
            , "You correctly guessed the word " ++ word'
            , " in " ++ show (length attempts') ++ " tries "
            ]
        TooManyWrongAttempts ->
            [ "YOU FAILED!"
            , "You failed to guess the word " ++ word'
            ]
    where
        fullHangmanImage' = fullHangmanImage currentHangmanIndex
        currentHangmanIndex = numberOfWrongAttempts wordState
        wordWithGuesses = blankOrChar <$> word'
        blankOrChar c
            | c `elem` attempts' = c
            | otherwise = '_'

-- | Try to guess a word
--
-- > >>> guessWord "dog"
-- > =========
-- > |    |
-- > |
-- > |
-- > |
-- > Word to guess: ___
-- >
-- > Guesses:
-- >
-- > Next char to guess
-- > <<< a
-- > a
-- >
-- > ...
-- >
-- > =========
-- > |    |
-- > |    O
-- > |    |
-- > |
-- > CONGRATULATIONS!
-- > You correctly guessed the word dog
-- > in 5 tries
-- >
guessWord :: String -> IO ()
guessWord word = guessLoop $ WordState word ""
   where
     guessLoop current@(WordState _word attempts) = do
       printWordState current
       when (lastAttemptOutcome current == GuessAnotherLetter) $ do
           c <- getAValidAttempt current
           guessLoop $ current { attempts = attempts ++ [c] }

-- | Gets a random word from a textfile
--
-- > >>> randomWord "words.txt"
-- > "cat"
-- > >>> randomWord "words.txt"
-- > "monkey"
randomWord :: FilePath -> IO String
randomWord wordsPath = do
    contents <- readFile wordsPath
    let words' = filter validWord $ lines contents
    let wordcount = length words'
    randomNumber <- randomRIO (0,wordcount-1)
    let randomWord = words' !! randomNumber
    return randomWord
    where
        validWord word =
            '\'' `notElem` word &&
            map toLower word == word

-- | Play the hangman game using random words from the specified file
--
-- > >>> hangmanGame "words.txt"
-- >
-- > ...
-- >
-- > YOU FAILED!
-- > You failed to guess the word lizard
-- >
-- > Play again? (y/n):
-- > y
-- >
-- > ...
-- >
-- > CONGRATULATIONS!
-- > You correctly guessed the word hippopotamus
-- > in 10 tries
-- >
-- > Play again? (y/n):
-- > <<< n
hangmanGame :: FilePath -> IO ()
hangmanGame wordsPath = do
    randomWord wordsPath >>= guessWord
    putStrLn "Play again? (y/n):"
    option <- getAChar
    when (option == 'y') $ hangmanGame wordsPath
