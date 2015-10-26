import Graphics.Gloss
import Data.List(transpose)

data Position = Position Int Int deriving (Eq, Show)
data Bounds = Bounds Position Position
data Positioned a = Positioned Position a deriving Show

data BlockColor = Red
                | Green
                | Blue
                | Yellow
                | Purple
                | Orange
                | Brown
                deriving (Bounded, Enum,Show)

data Block = Block BlockColor [Position] deriving Show
data Board = Board [Positioned BlockColor]

availableBlocks :: [Block]
availableBlocks = zipWith Block [minBound..] blockPositions
  where
    blockPositions = map parseBlock blocks
    parseBlock = positions . filterNonEmpty . listPositioned . list2dTo3Tuple
    list2dTo3Tuple = concat . zipWith (zip3 [0..] . repeat) [0..]
    listPositioned = map tupleToPositioned
    tupleToPositioned (x,y,c) = Positioned (Position x y) c
    positions = map (\(Positioned p _) -> p)
    filterNonEmpty = filter $ \(Positioned _ x) -> x /= ' '
    blocks = transpose
              [["xxxx","xx","xx "," xx","x  ","  x","xxx"]
              ,["    ","xx"," xx","xx ","xxx","xxx"," x "]
              ,["    ","  ","   ","   ","   ","   "," x "]]

main :: IO ()
main
 = display
        (InWindow
               "Hello World"     -- window title
                (400, 800)       -- window size
                (10, 10))        -- window position
        white                    -- background color
        picture                  -- picture to display

picture :: Picture
picture
        = Translate (-170) (-20) -- shift the text to the middle of the window
        $ Scale 0.5 0.5          -- display it half the original size
        $ Text "Hello World"     -- text to display
