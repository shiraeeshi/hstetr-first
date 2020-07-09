module Level
  (
    initlevel
    , nextFigure
    , Level ( .. )
  ) where

import System.Random
import Figure

data Level = Level StdGen

initlevel = Level (mkStdGen 100)

figuresToSelectFrom :: [Figure]
figuresToSelectFrom = [
    IShape (neighborLeft . neighborBelow $ center) False
    , IShape center True
    , IShape (neighborLeft . neighborBelow $ center) False
    , IShape center True
    , JShape center ToUp
    , JShape center ToRight
    , JShape center ToDown
    , JShape center ToLeft

    , LShape center ToUp
    , LShape center ToRight
    , LShape center ToDown
    , LShape center ToLeft

    , OShape (neighborLeft center)

    , SShape center True
    , SShape center False
    , SShape center True
    , SShape center False

    , TShape center ToUp
    , TShape center ToRight
    , TShape center ToDown
    , TShape center ToLeft

    , ZShape center True
    , ZShape center False
    , ZShape center True
    , ZShape center False
  ]
  where
  arenaWidth = 20
  arenaHeight = 20
  center = (arenaWidth `div` 2 - 1, arenaHeight - 2) :: Point

nextFigure :: Level -> (Figure, Level)
nextFigure (Level randomGen) =
  (randomFigure, Level newRandomGen)
  where
  len = length figuresToSelectFrom
  (randomIndex, newRandomGen) = randomR (0, len - 1) randomGen
  randomFigure = figuresToSelectFrom !! randomIndex
