module Engine(runEngine) where

import           Game
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

cellSize :: Int
cellSize = 30;

drawBlock :: Position -> Block -> Picture
drawBlock p b = Pictures $ map drawCell $ blockToCells p b

drawCell :: Cell -> Picture
drawCell (Cell blockcolor (Position xi yi)) =  Color (getColor blockcolor) poly
  where
    [x,y,cs] = map fromIntegral [xi*cellSize,yi*cellSize,cellSize-1]
    poly = Polygon [(x,y), (x+cs,y), (x+cs,y+cs), (x,y+cs)]
    getColor c = case c of
      Red -> red
      Green -> green
      Blue -> blue
      Yellow -> yellow
      Cyan -> cyan
      Orange -> orange
      Magenta -> magenta

drawLevel :: Level -> Picture
drawLevel (Level board state _score) =
  Pictures $ drawBoard board : drawState state
  where
    drawBoard (Board cells)= Pictures $ map drawCell cells
    drawState (BlockFalling pos block) = [drawBlock pos block]
    drawState (GameOver) =  [alignEtc 300 "Game over", alignEtc 250 "Press space to restart"]
    drawState _ = []
    alignEtc y = Color white . Translate 0 y . Scale 0.20 0.20 . Text

runEngine :: Level -> (LevelEvent -> Level -> IO Level) -> (Float -> Level -> IO Level) -> IO ()
runEngine wrld eventHandler =
  playIO window black
    3 -- simulations/sec
    wrld
    drawWrld
    internalEventHandler
  where
    window = InWindow "HaskTris by @ToJans" (w, h) (100, 100)
    w = cellSize * gridWidth
    h = cellSize * gridHeight
    drawWrld wrld = return
                $ translate (-fromIntegral w/2) (-fromIntegral h/2)
                $ drawLevel wrld
    internalEventHandler:: Event -> Level -> IO Level
    internalEventHandler evt = maybe return eventHandler $ maybeEvent evt
    maybeEvent evt = case evt of
        EventKey (SpecialKey KeyLeft)  Down _ _ -> Just MoveBlockLeft
        EventKey (SpecialKey KeyRight) Down _ _ -> Just MoveBlockRight
        EventKey (SpecialKey KeyUp)    Down _ _ -> Just RotateBlock
        EventKey (SpecialKey KeyDown)  Down _ _ -> Just MoveBlockDown
        EventKey (SpecialKey KeySpace) Down _ _ -> Just RestartGame
        otherwise -> Nothing
