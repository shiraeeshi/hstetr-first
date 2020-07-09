module Main where

import Arena
import Level
import ConsoleView
import Figure ( Figure (ZShape) )
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad (when)

main :: IO ()
main = do
  let
    emptyCells = take 20 (repeat (replicate 20 False))
    arena = Arena (ZShape (15,10) True) emptyCells 20 20
    -- arenaWrapper = ArenaWrapper arena \ar -> someAction ar
    level = initlevel
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  printArena 20 20 arena
  withArena level arena

withArena :: Level -> Arena -> IO ()
withArena level arena = do
  printArena 20 20 arena
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> do -- up
        withArena level (rotateCurrentFigureClockwise arena)
      "\ESC[B" -> do -- down
        let (newArena, descended) = descendCurrentFigure arena
        if descended
        then withArena level newArena
        else do
          let
            fixedFigureArena = fixCurrentFigure newArena
            (newFigure, newLevel) = nextFigure level
            newFigureArena = fst $ setFigureIfPossible newFigure fixedFigureArena
          withArena newLevel newFigureArena
      "\ESC[C" -> do -- right
        withArena level (moveCurrentFigureRight arena)
      "\ESC[D" -> do -- left
        withArena level (moveCurrentFigureLeft arena)
      "\n" ->
        return ()
      _ -> return ()

--------------------------------------------------------------------------------

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
