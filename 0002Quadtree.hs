-- A quadtree implementation  by @ToJans
--
-- This example fills a quadtree with 20 random points,
-- with a max of 3 points per node

module Main where

import           Control.Monad (replicateM)
import           System.Random (randomRIO)

data Point = Point Int Int
              deriving Show

type LeafPointCount = Int

data QuadTreeNode = Leaf [Point]
                  | Node { nNW :: QuadTreeNode
                         , nNE :: QuadTreeNode
                         , nSW :: QuadTreeNode
                         , nSE :: QuadTreeNode
                         }

data Bounds = Bounds { bLeft   :: Int
                     , bTop    :: Int
                     , bRight  :: Int
                     , bBottom :: Int
                     } deriving Show

data QuadTree = QuadTree { qtBounds        :: Bounds
                         , qtMaxLeafPoints :: LeafPointCount
                         , qtRoot          :: QuadTreeNode
                         }

instance Show QuadTree where
  show (QuadTree bnds maxc nodes) =  "Bounds: " ++ show bnds
                                  ++ "\nMax points per node: " ++ show maxc
                                  ++ "\nNodes:\n" ++ show nodes

instance Show QuadTreeNode where
     show (Leaf points) = "Leaf: " ++ show points
     show (Node nw ne sw se) = padl $ "\nNW: " ++ show nw
                                   ++ "\nNE: " ++ show ne
                                   ++ "\nSW: " ++ show sw
                                   ++ "\nSE: " ++ show se
              where padl = unlines . map ("  " ++) . lines

isInBounds :: Bounds -> Point -> Bool
isInBounds (Bounds l t r b) (Point x y) = x >= l && y >= t && x <= r && y <= b

subdivideBounds :: Bounds -> (Bounds,Bounds,Bounds,Bounds)
subdivideBounds (Bounds l t r b) =
                    ( Bounds l t cx cy
                    , Bounds cx t r cy
                    , Bounds l cy cx b
                    , Bounds cx cy r b
                    )
                    where cx = (l+r) `div` 2
                          cy = (t+b) `div` 2

emptyTree :: Bounds -> LeafPointCount -> QuadTree
emptyTree bounds maxLeafPoints = QuadTree bounds maxLeafPoints (Leaf [])

splitNode :: QuadTreeNode -> Bounds -> QuadTreeNode
splitNode (Leaf pts) bnds =
      let (nwb,neb,swb,seb) = subdivideBounds bnds in
      Node (leafInBounds nwb)
           (leafInBounds neb)
           (leafInBounds swb)
           (leafInBounds seb)
    where leafInBounds b = Leaf $ filter (isInBounds b) pts
splitNode other _ = other

pushPointToTree :: QuadTree -> Point -> QuadTree
pushPointToTree (QuadTree b maxc node) point =
          QuadTree b maxc $ maybeAddPointToNode node b
       where maybeAddPointToNode n bnds =
                if isInBounds bnds point
                   then addPointToNode n bnds
                   else n
             addPointToNode (Leaf pts) bnds =
                if length pts > maxc
                    then addPointToNode (splitNode (Leaf pts) bnds ) bnds
                    else Leaf $ point:pts
             addPointToNode (Node nw ne sw se) bnds =
                let (bnw,bne,bsw,bse) = subdivideBounds bnds in
                Node (maybeAddPointToNode nw bnw)
                     (maybeAddPointToNode ne bne)
                     (maybeAddPointToNode sw bsw)
                     (maybeAddPointToNode se bse)

pushPointsToTree :: QuadTree -> [Point] -> QuadTree
pushPointsToTree = foldl pushPointToTree

randomPoint :: Bounds -> IO Point
randomPoint (Bounds l t r b) = do
    x <- randomRIO (l, r)
    y <- randomRIO (t, b)
    return (Point x y)

randomPoints :: Bounds -> Int -> IO [Point]
randomPoints bnds cnt = replicateM cnt $ randomPoint bnds

main :: IO ()
main = do
  let bounds = Bounds 0 0 1000 1000
  let emptytree = emptyTree bounds 3
  points <- randomPoints bounds 20
  let tree = pushPointsToTree emptytree points
  print tree

-- OUTPUT
-- E:\Dev\haskell\learning>runghc "0002 Quadtree.hs"
-- 0002 Quadtree.hs: warning: _tzset from msvcrt is linked instead of __imp__tzset
-- Bounds: Bounds {bLeft = 0, bTop = 0, bRight = 1000, bBottom = 1000}
-- Max points per node: 3
-- Nodes:
--
--   NW: Leaf: [Point 196 178,Point 86 330]
--   NE:
--     NW: Leaf: [Point 560 43,Point 618 28,Point 550 0,Point 576 185]
--     NE: Leaf: [Point 817 78]
--     SW: Leaf: [Point 593 371]
--     SE: Leaf: [Point 946 339,Point 792 313,Point 830 364]
--
--   SW:
--     NW: Leaf: []
--     NE: Leaf: [Point 346 515,Point 378 708]
--     SW: Leaf: [Point 28 951,Point 51 904]
--     SE: Leaf: [Point 366 807]
--
--   SE: Leaf: [Point 721 526,Point 944 685,Point 502 796,Point 770 661]
--
--
-- E:\Dev\haskell\learning>
