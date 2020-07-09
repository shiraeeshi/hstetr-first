module ConsoleView
  ( printArena
  ) where

import Arena
import Figure
import System.Console.ANSI
import Control.Monad (when)

printArena :: Int -> Int -> Arena -> IO ()
printArena arenaWidth arenaHeight arena = do
  saveCursor
  clearScreen
  drawBox 0 0 (arenaWidth + 2) (arenaHeight + 2)
  drawFigure arenaHeight (figure arena)
  drawBricks arenaHeight (getBricksOnTheFloor arena)
  when (hasFullRows arena) (drawFullRows arenaWidth arenaHeight . findFullRowsIndices $ arena)

drawFigure :: Int -> Figure -> IO ()
drawFigure arenaHeight figure = do
  mapM_ printPoint (getBricks figure)
  restoreCursor
  where
  printPoint (x, y) = do
    setCursorPosition (arenaHeight - y + 3) (x + 1)
    putStr "*"

drawBricks :: Int -> [(Int, Int)] -> IO ()
drawBricks arenaHeight bricks = do
  mapM_ printPoint bricks
  restoreCursor
  where
  printPoint (x, y) = do
    setCursorPosition (arenaHeight - y + 3) (x + 1)
    putStr "*"

drawFullRows :: Int -> Int -> [Int] -> IO ()
drawFullRows arenaWidth arenaHeight fullRowsIndices = do
  mapM_ printMinus [(rowIndex, columnIndex) | rowIndex <- fullRowsIndices, columnIndex <- [0 .. arenaWidth-1]]
  restoreCursor
  where
  printMinus (rowIndex, columnIndex) = do
    setCursorPosition (arenaHeight - rowIndex + 3) (columnIndex + 1)
    putStr "-"

drawBox :: Int -> Int -> Int -> Int -> IO ()
drawBox x0 y0 w h = do

  setCursorPosition (y0+2) x0
  putStr topStr

  mapM_ printMiddle [0 .. h-1]

  setCursorPosition (y0+2+h) x0
  putStr bottomStr

  where
  lineStr   = replicate (w-2) '─'
  emptStr   = replicate (w-2) ' '
  topStr    = "┌" ++ lineStr ++ "┐"
  middleStr = "│" ++ emptStr ++ "│"
  bottomStr = "└" ++ lineStr ++ "┘"
  printMiddle row = do
    setCursorPosition (y0+3+row) x0
    putStr middleStr
