-- Sudoku solver by @ToJans
--
-- Parses a Sudoku from a string and solves it
-- Parsing happens by filtering out ['1'-'9'] and '.'

{-# LANGUAGE OverloadedStrings #-}

module Sudoko where

import Data.Maybe(isNothing,fromMaybe)
import Data.List(sortOn,find)

newtype Value = Value Char deriving (Show,Eq)

data Board = Board [[Maybe Value]]

data Position = Position Int Int deriving Show

data PotentialTurn = PotentialTurn Position [Value] deriving Show

data Turn = Turn Position Value deriving Show

data Range = Range [Position] deriving Show

data StepOutcome = BoardSolved Board
                 | BoardUnsolvable Board
                 | PotentialBoards [Board]
                 deriving Show

allValidChars :: String
allValidChars = ['1'..'9']

allValues :: [Value]
allValues = map Value allValidChars

charToMaybeValue :: Char -> Maybe Value
charToMaybeValue c = if c `elem` allValidChars
                      then Just $ Value c
                      else Nothing

maybeValueToChar :: Maybe Value -> Char
maybeValueToChar (Just (Value v)) = v
maybeValueToChar Nothing = '.'

stringToBoard :: String -> Board
stringToBoard x = if length filteredChars /= 9*9
                        then error "Invalid board"
                        else Board $ partition 9 filteredValues
            where allBoardChars = '.' : allValidChars
                  filteredChars = filter (`elem` allBoardChars) x
                  filteredValues = map charToMaybeValue filteredChars
                  partition _ [] = []
                  partition n xs = take n xs : partition n (drop n xs)

instance Show Board where
  show (Board b) = unlines $ "" :
                    fullLine ++
                    map showLine [0..2] ++
                    fullLine ++
                    map showLine [3..5] ++
                    fullLine ++
                    map showLine [6..8] ++
                    fullLine
          where fullLine = [ "|-----------------------|" ]
                showLine y = foldl (\acc x -> acc ++ showEl x y) "" [0..8] ++ "|"
                showEl x y = prefix ++ (ch:" ")
                  where prefix = if x `mod` 3 == 0 then "| " else ""
                        mv = valueFromPosition (Board b) (Position x y)
                        ch = maybeValueToChar mv

rangesForPosition :: Position -> [Range]
rangesForPosition (Position x y) = [rangeRow,rangeCol,range3By3]
  where rangeRow = Range $ map (`Position` y ) [0..8]
        rangeCol = Range $ map ( Position  x ) [0..8]
        range3By3Left = (x `div` 3) * 3
        range3By3Top = (y `div` 3) * 3
        range3By3 = Range $ Position
                            <$> [range3By3Left..range3By3Left+2]
                            <*> [range3By3Top ..range3By3Top +2]

valueFromPosition :: Board -> Position -> Maybe Value
valueFromPosition (Board b) (Position x y) =  b !! y !! x

isValidValueForRange :: Board -> Range -> Value -> Bool
isValidValueForRange b (Range positions) v = Just v `notElem` values
    where values = map (valueFromPosition b) positions

isValidValueForPosition :: Board -> Position -> Value -> Bool
isValidValueForPosition b p v = all (\r -> isValidValueForRange b r v) ranges
    where ranges = rangesForPosition p

validValuesForPosition :: Board -> Position -> [Value]
validValuesForPosition b p = filter (isValidValueForPosition b p) allValues

positionsToGuess :: Board -> [Position]
positionsToGuess b = filter (isNothing . valueFromPosition b ) allPositions
  where allPositions = Position <$> [0..8] <*> [0..8];

potentialTurn :: Board -> Position -> PotentialTurn
potentialTurn b p = PotentialTurn p $ validValuesForPosition b p

isPotentialTurnWithoutValues :: PotentialTurn -> Bool
isPotentialTurnWithoutValues (PotentialTurn _ []) = True
isPotentialTurnWithoutValues _ = False

applyTurn :: Board -> Turn -> Board
applyTurn (Board b) (Turn (Position x y) newVal) =
       Board $ replace y rowToReplace b
    where replace pos nv list = take pos list ++ nv : drop (pos+1) list
          rowToReplace = replace x (Just newVal) $ b !! y

applyPotentialTurnsToBoard :: Board -> PotentialTurn -> [Board]
applyPotentialTurnsToBoard b (PotentialTurn pos vals) =
      map getBoardForTurnVal vals
    where getBoardForTurnVal v = applyTurn b (Turn pos v)

nextStep :: Board -> StepOutcome
nextStep b
  | isBoardSolved = BoardSolved b
  | isNotSolvable = BoardUnsolvable b
  | otherwise     = PotentialBoards potentialBoards
  where positionsToGuess' = positionsToGuess b
        isBoardSolved = null positionsToGuess'
        potentialTurns = map (potentialTurn b) positionsToGuess'
        potentialTurnLength (PotentialTurn _p v) = length v
        sortedPotentialTurns = sortOn potentialTurnLength potentialTurns
        -- we only need to walk the paths with the least possibilities
        potentialBoards = applyPotentialTurnsToBoard b $ head sortedPotentialTurns
        unsolvablePoints = filter isPotentialTurnWithoutValues potentialTurns
        isNotSolvable = not $ null unsolvablePoints

isSolution :: StepOutcome -> Bool
isSolution x = case x of
  BoardSolved _ -> True
  _ -> False

isUnsolvable :: StepOutcome -> Bool
isUnsolvable x = case x of
  BoardUnsolvable _ -> True
  _ -> False

stepToBoards :: StepOutcome -> [Board]
stepToBoards (PotentialBoards bs) = bs
stepToBoards _ = []

solve :: Board -> StepOutcome
solve b = case nextStep b of
            PotentialBoards bs -> solveMultiple bs
            BoardSolved bs     -> BoardSolved bs
            BoardUnsolvable bs -> BoardUnsolvable bs
          where
            solveMultiple [] = BoardUnsolvable b
            solveMultiple bs =
                  let ns = filter (not . isUnsolvable) $ map nextStep bs in
                  fromMaybe
                    (solveMultiple $ concatMap stepToBoards ns)
                    (find isSolution ns)

easyboard :: Board
easyboard = stringToBoard
            "|-----------------------|\n\
            \| 4 . 3 | 1 . . | 9 . 8 |\n\
            \| . 2 9 | . . . | 4 5 . |\n\
            \| 1 7 . | 8 4 9 | . . . |\n\
            \|-----------------------|\n\
            \| . 4 . | . 2 . | . . . |\n\
            \| . 9 . | 4 . 7 | . 3 . |\n\
            \| . . . | . 3 . | . 8 . |\n\
            \|-----------------------|\n\
            \| . . . | 5 9 8 | . 4 7 |\n\
            \| . 5 7 | . . . | 8 6 . |\n\
            \| 2 . 4 | . . 3 | 5 . 9 |\n\
            \|-----------------------|\n"

evilboard :: Board
evilboard = stringToBoard
    "|-----------------------|\n\
    \| . . . | . 4 5 | . . . |\n\
    \| 8 . 6 | . 2 . | . 3 . |\n\
    \| . . 2 | . . 8 | . . 6 |\n\
    \|-----------------------|\n\
    \| . 7 . | . . . | . 1 . |\n\
    \| 9 . . | . 8 . | . . 3 |\n\
    \| . 1 . | . . . | . 9 . |\n\
    \|-----------------------|\n\
    \| 3 . . | 9 . . | 5 . . |\n\
    \| . 4 . | . 6 . | 3 . 7 |\n\
    \| . . . | 8 3 . | . . . |\n\
    \|-----------------------|\n"


main :: IO ()
main = do
  putStrLn "Sudoku solver by @ToJans"
  putStrLn "Solving easy Sudoku"
  print easyboard
  print $ solve easyboard
  putStrLn "Solving evil sudoku"
  print evilboard
  print $ solve evilboard


-- output:
-- E:\Dev\haskell\learning>runhaskell "0003 Sudoku.hs"
-- Sudoku solver by @ToJans
--
-- |-----------------------|
-- | 4 . 3 | 1 . . | 9 . 8 |
-- | . 2 9 | . . . | 4 5 . |
-- | 1 7 . | 8 4 9 | . . . |
-- |-----------------------|
-- | . 4 . | . 2 . | . . . |
-- | . 9 . | 4 . 7 | . 3 . |
-- | . . . | . 3 . | . 8 . |
-- |-----------------------|
-- | . . . | 5 9 8 | . 4 7 |
-- | . 5 7 | . . . | 8 6 . |
-- | 2 . 4 | . . 3 | 5 . 9 |
-- |-----------------------|
--
-- Finding solution
-- BoardSolved
-- |-----------------------|
-- | 4 6 3 | 1 5 2 | 9 7 8 |
-- | 8 2 9 | 3 7 6 | 4 5 1 |
-- | 1 7 5 | 8 4 9 | 3 2 6 |
-- |-----------------------|
-- | 3 4 8 | 6 2 1 | 7 9 5 |
-- | 5 9 6 | 4 8 7 | 1 3 2 |
-- | 7 1 2 | 9 3 5 | 6 8 4 |
-- |-----------------------|
-- | 6 3 1 | 5 9 8 | 2 4 7 |
-- | 9 5 7 | 2 1 4 | 8 6 3 |
-- | 2 8 4 | 7 6 3 | 5 1 9 |
-- |-----------------------|
--
--
-- E:\Dev\haskell\learning>
