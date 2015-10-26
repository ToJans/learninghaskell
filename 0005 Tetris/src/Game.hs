module Game where

import Data.List(transpose)
import System.Random(randomRIO)

data Position = Position Int Int deriving (Eq, Show)

data BlockColor = Red
                | Green
                | Blue
                | Yellow
                | Cyan
                | Orange
                | Magenta
                deriving (Bounded, Enum, Eq, Show)

data Cell = Cell BlockColor Position deriving (Eq,Show)

data Block = Block BlockColor [Position] deriving Show

data Board = Board [Cell]

data State = NewBlock
           | GameOver
           | BlockFalling Position Block
           deriving Show

data Level = Level {
                    lBoard :: Board,
                    lState :: State,
                    lScore :: Int
                  }

data LevelEvent = MoveLeft | MoveRight | MoveDown | MoveUp

gridWidth :: Int
gridWidth = 10

gridHeight :: Int
gridHeight = 20


initLevel :: Level
initLevel = Level emptyBoard NewBlock 0
  where
    emptyBoard = Board []

startPos :: Position
startPos = Position (gridWidth `div` 2) (gridHeight - 1)

-- Main loop
timeStepHandler :: Float -> Level -> IO Level
timeStepHandler _ts lvl@(Level board@(Board cells) state score) =
  case state of
    GameOver -> return lvl
    NewBlock -> do
      newBlock <- randomBlock
      return $ maybe (lvlState GameOver) blockFalling $ validate board startPos newBlock
    BlockFalling p@(Position x y) bl->
      return $ maybe (mergeblock p bl) blockFalling $ validate board (Position x (y-1)) bl
    where
      lvlState x = lvl {lState = x}
      blockFalling (position,block) = lvlState $ BlockFalling position block
      mergeblock p b= lvl {lBoard = Board $ removeFullRows $ cells ++ blockToCells p b, lState = NewBlock}

-- Main event handler
eventHandler :: LevelEvent -> Level -> IO Level
-- only works when a block is falling
eventHandler evt lvl@(Level board (BlockFalling pos@(Position x y) block) score) =
    return $ maybe lvl blockFalling $ validate board p b
    where
      (p,b) = case evt of
        MoveLeft  -> (Position (x-1) y   , block)
        MoveRight -> (Position (x+1) y   , block)
        MoveDown  -> (Position x    (y-1), block)
        MoveUp    -> (Position x     y   , rotateBlock block)
      lvlState x = lvl {lState = x}
      blockFalling (position,block) = lvlState $ BlockFalling position block
      rotateBlock (Block color positions) = Block color $ map rotatePosition positions
      rotatePosition (Position x y) = Position y (-x)
eventHandler _ level = return level

-- Returns a (Position,Block) if it is valid, otherwise Nothing
validate :: Board -> Position -> Block -> Maybe (Position,Block)
validate (Board cells) pos@(Position dx dy) bl@(Block color positions) =
  if any invalidPosition movedPositions
    then Nothing
    else Just (pos,bl)
  where
    movedPositions = map addPos positions
    addPos (Position x y) = Position (x+dx) (y+dy)
    invalidPosition p@(Position x y) = x < 0 || x > gridWidth - 1 || y < 0 || p `elem` cellPositions
    cellPositions = map cellPosition cells
    cellPosition (Cell _c p) = p

-- Remove all rows that are complete and reindex the remaining rows
removeFullRows :: [Cell] -> [Cell]
removeFullRows cells = remapRows $ map selectIfNotFullRow [0..gridHeight-1]
  where
    selectRow r = filter (isCellInRow r) cells
    isCellInRow r (Cell _ (Position _x y)) = r == y
    selectIfNotFullRow r =
      let sr = selectRow r in
      if length sr == gridWidth
        then []
        else sr
    remapRows cells = concat $ zipWith (\i cls -> map (remapCell i) cls) [0..] $ filter (not . null) cells
    remapCell i (Cell color (Position x _))= Cell color (Position x i)

-- Convert a Block into board Cells
blockToCells :: Position -> Block -> [Cell]
blockToCells (Position dx dy) (Block clr positions) =
  map (Cell clr . movePos) positions
  where
    movePos (Position x y) = Position (x+dx) (y+dy)

-- Return a random Block
randomBlock :: IO Block
randomBlock = do
  index <- randomRIO (0, length availableBlocks - 1)
  return $ availableBlocks !! index
  where
    availableBlocks = zipWith Block [minBound..] blockPositions
    blockPositions = map parseBlock blocks
    parseBlock = map tupleToPosition . filter notEmpty . list2dTo3Tuple
    list2dTo3Tuple = concat . zipWith (zip3 [0..] . repeat) [0..]
    tupleToPosition (x,y,_c) = Position (x - 1) (y-1)
    notEmpty (_x,_y,'█') = True
    notEmpty _ = False
    blocks = transpose . map words $
              [ "_█ ██ ██ ███ ██ ██_ _██"
              , "_█ █_ _█ _█_ ██ _██ ██"
              , "_█ █_ _█"
              , "_█"
              ]
