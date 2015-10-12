-- Hangman game by @ToJans
--
-- Reads a random word from the file words.txt
-- Allows you to guess the word letter by letter

import           Control.Monad (when)
import           Data.Char     (toLower)
import           Data.List     (transpose)
import           System.Random (randomIO)

wordsPath :: FilePath
wordsPath = "0001 words.txt"-- "/usr/share/dict/words"

data GameState = GameState
    { _wordToGuess :: String
    , guesses      :: String
    }

data GameStatus = Guessing | GameWon | GameLost deriving Eq

hangmanImages :: [[String]]
hangmanImages =
    transpose
    [ [ "   ", " O ", " O ", " O ", " O " , "_O " , "_O_"  ]
    , [ "   ", "   ", " | ", " | ", " | " , " | " , " | "  ]
    , [ "   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\" ]
    ]

fullHangmanImage :: Int -> [String]
fullHangmanImage index =
    "=========" :
    "|    |" :
    map ("|   " ++) img
    where img = hangmanImages !! index


maxWrongGuesses :: Int
maxWrongGuesses = length hangmanImages - 1

numberOfWrongGuesses :: GameState -> Int
numberOfWrongGuesses (GameState word' guesses') =
    length $ filter charNotInWord guesses'
    where charNotInWord c = c `notElem` word'

gameStatus :: GameState -> GameStatus
gameStatus (GameState word' guesses')
    | isGuessed   = GameWon
    | isLastGuess = GameLost
    | otherwise   = Guessing
    where
        isGuessed = all isCharInGuesses word'
        isCharInGuesses x = x `elem` guesses'
        gameState = GameState word' guesses'
        isLastGuess = numberOfWrongGuesses gameState == maxWrongGuesses

-- for one reason or another getChar also appends <CR>
-- so I implemented my own getChar and made sure empty input is refused
getAChar :: IO Char
getAChar = do
    line <- getLine
    case line of
        [] -> getAChar
        (c:_) -> return c

getANewChar :: GameState -> IO Char
getANewChar gameState = do
    putStrLn "Next char to guess"
    c <- getAChar
    if c `elem` guesses gameState
    then do
        putStrLn "Character already used in guesses."
        getANewChar gameState
    else
        return c

displayState :: GameState -> IO ()
displayState (GameState word' guesses')  =
    putStrLn $ unlines $ fullHangmanImage' ++ case gameStatus gameState of
        Guessing ->
            [ "Word to guess: " ++ wordWithGuesses
            , ""
            , "Guesses: " ++ guesses'
            ]
        GameWon ->
            [ "CONGRATULATIONS!"
            , "You correctly guessed the word " ++ word'
            , " in " ++ show (length guesses') ++ " tries "
            ]
        GameLost ->
            [ "YOU FAILED!"
            , "You failed to guess the word " ++ word'
            ]
    where
        gameState = GameState word' guesses'
        fullHangmanImage' = fullHangmanImage currentHangmanIndex
        currentHangmanIndex = numberOfWrongGuesses gameState
        wordWithGuesses = blankOrChar <$> word'
        blankOrChar c
            | c `elem` guesses' = c
            | otherwise = '_'

gameLoop :: GameState -> IO ()
gameLoop gameState = do
    displayState gameState
    when (gameStatus gameState == Guessing) $ do
        c <- getANewChar gameState
        gameLoop $ gameState { guesses = guesses gameState ++ [c] }

newGame :: IO GameState
newGame = do
    contents <- readFile wordsPath
    let words' = filter validWord $ lines contents
    let wordcount = length words'
    randomNumber <- randomIO
    let randomWord = words' !! (randomNumber `mod` wordcount)
    return $ GameState randomWord []
    where
        validWord word =
            '\'' `notElem` word &&
            map toLower word == word

main :: IO ()
main = do
    newGame >>= gameLoop
    putStrLn "Play again? (y/n):"
    option <- getAChar
    when (option == 'y') main
