module Figure
  ( Point
  , neighborLeft
  , neighborRight
  , neighborAbove
  , neighborBelow

  , Direction (..)
  , Figure (..)

  , rotateClockwise
  , moveLeft
  , moveRight
  , descend
  , getBricks
  ) where

type Point = (Int, Int)

data Direction = ToLeft | ToRight | ToUp | ToDown deriving Show

data Figure
  = IShape {center :: Point, isVertical :: Bool}
  | JShape {center :: Point, direction :: Direction}
  | LShape {center :: Point, direction :: Direction} 
  | OShape {center :: Point} 
  | SShape {center :: Point, isVertical :: Bool} 
  | TShape {center :: Point, direction :: Direction} 
  | ZShape {center :: Point, isVertical :: Bool} deriving Show

rotateDirectionClockwise :: Direction -> Direction
rotateDirectionClockwise ToUp = ToRight
rotateDirectionClockwise ToRight = ToDown
rotateDirectionClockwise ToDown = ToLeft
rotateDirectionClockwise ToLeft = ToUp

neighborLeft :: Point -> Point
neighborLeft (x, y) = (x - 1, y)

neighborRight :: Point -> Point
neighborRight (x, y) = (x + 1, y)

neighborAbove :: Point -> Point
neighborAbove (x, y) = (x, y + 1)

neighborBelow :: Point -> Point
neighborBelow (x, y) = (x, y - 1)


rotateClockwise :: Figure -> Figure
rotateClockwise ( IShape center isVertical ) =
  let newCenter = if isVertical
      then neighborLeft . neighborBelow $ center
      else neighborRight . neighborAbove $ center
  in
  IShape {center = newCenter, isVertical = not isVertical}
rotateClockwise (JShape center direction) = JShape center (rotateDirectionClockwise direction)
rotateClockwise (LShape center direction) = LShape center (rotateDirectionClockwise direction)
rotateClockwise (OShape center) = OShape center
rotateClockwise (SShape center isVertical) = SShape center (not isVertical)
rotateClockwise (TShape center direction) = TShape center (rotateDirectionClockwise direction)
rotateClockwise (ZShape center isVertical) = ZShape center (not isVertical)

moveLeft :: Figure -> Figure
moveLeft ( IShape center isVertical ) =
  IShape (neighborLeft center) isVertical
moveLeft (JShape center direction) = JShape (neighborLeft center) direction
moveLeft (LShape center direction) = LShape (neighborLeft center) direction
moveLeft (OShape center) = OShape (neighborLeft center)
moveLeft (SShape center isVertical) = SShape (neighborLeft center) isVertical
moveLeft (TShape center direction) = TShape (neighborLeft center) direction
moveLeft (ZShape center isVertical) = ZShape (neighborLeft center) isVertical

moveRight :: Figure -> Figure
moveRight ( IShape center isVertical ) =
  IShape (neighborRight center) isVertical
moveRight (JShape center direction) = JShape (neighborRight center) direction
moveRight (LShape center direction) = LShape (neighborRight center) direction
moveRight (OShape center) = OShape (neighborRight center)
moveRight (SShape center isVertical) = SShape (neighborRight center) isVertical
moveRight (TShape center direction) = TShape (neighborRight center) direction
moveRight (ZShape center isVertical) = ZShape (neighborRight center) isVertical

descend :: Figure -> Figure
descend ( IShape center isVertical ) =
  IShape (neighborBelow center) isVertical
descend (JShape center direction) = JShape (neighborBelow center) direction
descend (LShape center direction) = LShape (neighborBelow center) direction
descend (OShape center) = OShape (neighborBelow center)
descend (SShape center isVertical) = SShape (neighborBelow center) isVertical
descend (TShape center direction) = TShape (neighborBelow center) direction
descend (ZShape center isVertical) = ZShape (neighborBelow center) isVertical

getBricks :: Figure -> [Point]
getBricks ( IShape center isVertical ) =
  if isVertical
  then [
    neighborAbove center,
    center,
    neighborBelow center,
    neighborBelow . neighborBelow $ center
  ]
  else [
    neighborLeft center,
    center,
    neighborRight center,
    neighborRight . neighborRight $ center
  ]
getBricks (JShape center direction) =
  case direction of ToUp -> [
                              neighborAbove center,
                              center,
                              neighborBelow center,
                              neighborLeft . neighborBelow $ center
                            ]
                    ToRight -> [
                              neighborRight center,
                              center,
                              neighborLeft center,
                              neighborAbove . neighborLeft $ center
                            ]
                    ToDown -> [
                              neighborBelow center,
                              center,
                              neighborAbove center,
                              neighborRight . neighborAbove $ center
                            ]
                    ToLeft -> [
                              neighborLeft center,
                              center,
                              neighborRight center,
                              neighborRight . neighborBelow $ center
                            ]
getBricks (LShape center direction) =
  case direction of ToUp -> [
                              neighborAbove center,
                              center,
                              neighborBelow center,
                              neighborRight . neighborBelow $ center
                            ]
                    ToRight -> [
                              neighborRight center,
                              center,
                              neighborLeft center,
                              neighborBelow . neighborLeft $ center
                            ]
                    ToDown -> [
                              neighborBelow center,
                              center,
                              neighborAbove center,
                              neighborLeft . neighborAbove $ center
                            ]
                    ToLeft -> [
                              neighborLeft center,
                              center,
                              neighborRight center,
                              neighborAbove . neighborRight $ center
                            ]
getBricks (OShape center) = [
    center,
    neighborRight center,
    neighborBelow center,
    neighborBelow . neighborRight $ center
  ]
getBricks (SShape center isVertical) =
  if isVertical
  then [
    neighborAbove center,
    center,
    neighborRight center,
    neighborBelow . neighborRight $ center
  ]
  else [
    neighborRight center,
    center,
    neighborBelow center,
    neighborLeft . neighborBelow $ center
  ]
getBricks (TShape center direction) =
  case direction of ToUp -> [
                              center,
                              neighborRight center,
                              neighborLeft center,
                              neighborAbove center
                            ]
                    ToRight -> [
                              center,
                              neighborAbove center,
                              neighborBelow center,
                              neighborRight center
                            ]
                    ToDown -> [
                              center,
                              neighborLeft center,
                              neighborRight center,
                              neighborBelow center
                            ]
                    ToLeft -> [
                              center,
                              neighborBelow center,
                              neighborAbove center,
                              neighborLeft center
                            ]
getBricks (ZShape center isVertical) =
  if isVertical
  then [
    neighborBelow center,
    center,
    neighborRight center,
    neighborAbove . neighborRight $ center
  ]
  else [
    neighborLeft center,
    center,
    neighborBelow center,
    neighborRight . neighborBelow $ center
  ]
