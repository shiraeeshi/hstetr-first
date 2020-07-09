module Arena
  ( Arena (..)
  , moveCurrentFigureRight
  , moveCurrentFigureLeft
  , rotateCurrentFigureClockwise
  , descendCurrentFigure
  , fixCurrentFigure
  , setFigureIfPossible
  , getBricksOnTheFloor
  , hasFullRows
  , findFullRowsIndices
  , removeFullRows
  ) where

import Figure
  ( Figure
  , Point
  , moveRight
  , moveLeft
  , rotateClockwise
  , descend
  , getBricks
  )

data Arena = Arena {figure :: Figure, cells :: [[Bool]], width :: Int, height :: Int}

getBricksOnTheFloor :: Arena -> [Point]
getBricksOnTheFloor (Arena _ cells _ _) =
  map snd taken
  where
  taken = filter (fst) cellsWithIndices
  rowWithIndexToTrio (row, y) = [(value, (x, y)) | (value, x) <- zip row [0..]]
  listsOfTrios = map rowWithIndexToTrio (zip cells [0..])
  cellsWithIndices = foldr (++) [] listsOfTrios

moveCurrentFigureRight :: Arena -> Arena
moveCurrentFigureRight arena = fst $ setFigureIfPossible (moveRight (figure arena)) arena

moveCurrentFigureLeft :: Arena -> Arena
moveCurrentFigureLeft arena = fst $ setFigureIfPossible (moveLeft (figure arena)) arena

rotateCurrentFigureClockwise :: Arena -> Arena
rotateCurrentFigureClockwise arena = fst $ setFigureIfPossible (rotateClockwise (figure arena)) arena

descendCurrentFigure :: Arena -> (Arena, Bool)
descendCurrentFigure arena = setFigureIfPossible (descend (figure arena)) arena

fixCurrentFigure :: Arena -> Arena
fixCurrentFigure (Arena figure cells w h) = Arena figure newCells w h
  where
  newCells = foldr updateBools cells bricks
  bricks = getBricks figure

updateBools (x, y) cells = (take y cells) ++ [newRow] ++ (drop (y+1) cells)
  where
  oldRow = cells !! y
  newRow = (take x oldRow) ++ [True] ++ (drop (x+1) oldRow)

setFigureIfPossible :: Figure -> Arena -> (Arena, Bool)
setFigureIfPossible figure arena =
  if figureIsPossible figure arena
  then let newArena = Arena figure (cells arena) (width arena) (height arena)
       in (newArena, True)
  else (arena, False)

figureIsPossible figure arena =
  not $ any isImpossible (getBricks figure)
  where
  isImpossible (x,y) | x < 0 = True
                     | x > (width arena) - 1 = True
                     | y < 0 = True
                     | y > (height arena) - 1 = True
                     | (cells arena) !! y !! x = True
                     | otherwise = False

hasFullRows :: Arena -> Bool
hasFullRows arena = any isFull (cells arena)
  where
  isFull row = and row

findFullRowsIndices :: Arena -> [Int]
findFullRowsIndices arena = map snd fullRowsWithIndices
  where
  fullRowsWithIndices = filter isFull (zip (cells arena) [0..])
  isFull (row, _) = and row

removeFullRows :: Arena -> Arena
removeFullRows (Arena figure cells width height) = Arena figure newCells width height
  where
  newCells = take height ((filter isNotFull cells) ++ (repeat (replicate width False)))
  isNotFull row = not $ and row

