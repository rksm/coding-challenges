module Botcleanr where

import Data.List
import Data.Sequence hiding (filter, length, replicate)

data Direction = UP | DOWN | LEFT | RIGHT | STAY deriving (Show, Eq)
data Action = CLEAN | Go Direction deriving (Show, Eq)
type Pos = (Int,Int)
data Field = Field
            { width :: Int
            , height :: Int
            , botPos :: Pos
            , dirtyCells :: [Pos]
            } deriving (Show, Eq)

delta :: Pos -> Pos -> Pos
delta (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

dist :: Pos -> Double
dist (x, y) = sqrt $ fromIntegral (x^2) + fromIntegral (y^2)

stepTo :: Pos -> Pos -> Pos
stepTo a b =
  let (x, y) = delta a b
      distTo = dist (x, y)
      dx = fromIntegral x / distTo
      dy = fromIntegral y / distTo
      dx' = round dx
      dy' = round dy
  in if (abs dx' >= 1) && (abs dy' >= 1)
     then if dx > dy then (dx', 0) else (0, dy')
     else (dx', dy')

stepToWithDir :: Pos -> Pos -> (Pos, Direction)
stepToWithDir a b =
  case delta of
    (-1, 0) -> (delta, LEFT)
    (1, 0) -> (delta, RIGHT)
    (0, -1) -> (delta, UP)
    (0, 1) -> (delta, DOWN)
    _ -> error "should not happen"
  where delta = stepTo a b

compareDist :: Pos -> Pos -> Pos -> Ordering
compareDist from a b =
  let distA = dist $ delta from a
      distB = dist $ delta from b
  in compare distA distB

whatToDo :: Field -> Action
whatToDo (Field w h pos []) = error "Nothing dirty"
whatToDo (Field w h pos dirty) =
  if any (== pos) dirty
  then CLEAN
  else
    let target = minimumBy (compareDist pos) dirty
        (step, dir) = stepToWithDir pos target
    in Go dir

readString :: String -> Field
readString string =
    let allLines = lines string
        (posString:fieldLines) = allLines
        (y:x:_) = (map (read :: String -> Int) . words) posString
        pos = (x, y)
        width = (length . head) fieldLines
        height = length fieldLines
        field = readFieldString width height 0 fieldLines pos []
    in field

readFieldString width height row [] pos dirty = Field width height pos dirty
readFieldString width height row (line:lines) pos dirty =
  let dirty' = foldlWithIndex
               (\dirty i char -> if char == 'd'
                                 then (i, row) : dirty
                                 else dirty)
               dirty
               (fromList line)
  in readFieldString width height (row + 1) lines pos dirty'
