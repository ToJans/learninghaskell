module Draw(myPlay) where

import           Game
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

cellSize :: Int
cellSize = 40;

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
    drawState _ = []

myPlay :: Level -> (LevelEvent -> Level -> IO Level) -> (Float -> Level -> IO Level) -> IO ()
myPlay wrld eventHandler =
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
        EventKey (SpecialKey KeyLeft) Down _ _  -> Just MoveLeft
        EventKey (SpecialKey KeyRight) Down _ _ -> Just MoveRight
        EventKey (SpecialKey KeyUp) Down _ _  -> Just MoveUp
        EventKey (SpecialKey KeyDown) Down _ _ -> Just MoveDown
        otherwise -> Nothing
