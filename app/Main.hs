module Main where

import Arena
import Level
import ConsoleView
import Figure ( Figure (ZShape) )
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad (when, foldM)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Timer
import Control.Concurrent.Chan
import Control.Concurrent.Suspend.Lifted (msDelay)

data TetrisCommand = CmdRotate | CmdDescend | CmdRight | CmdLeft | CmdTick | CmdPauseOrResume

data TetrisState = TetrisStateRunning Arena Level (Chan TetrisCommand) TimerIO | TetrisStatePaused Arena Level (Chan TetrisCommand)

getArenaFromState :: TetrisState -> Arena
getArenaFromState (TetrisStateRunning arena _ _ _) = arena
getArenaFromState (TetrisStatePaused arena _ _) = arena

stateWithNewArena :: TetrisState -> Arena -> TetrisState
stateWithNewArena (TetrisStateRunning _ l c t) arena = TetrisStateRunning arena l c t
stateWithNewArena (TetrisStatePaused _ l c) arena = TetrisStatePaused arena l c

main :: IO ()
main = do
  let
    emptyCells = take 20 (repeat (replicate 20 False))
    arena = Arena (ZShape (15,10) True) emptyCells 20 20
    level = initlevel
  chan <- newChan
  timer <- repeatedTimer (timerTick chan) (msDelay 300)
  let
    arenaWithLevel = TetrisStateRunning arena level chan timer
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  printArena 20 20 arena
  concurrently (keepHandlingTetrisCommands arenaWithLevel chan) (keys2commands chan)
  return ()

timerTick :: Chan TetrisCommand -> IO ()
timerTick chan = do
  writeChan chan CmdTick

keepHandlingTetrisCommands :: TetrisState -> Chan TetrisCommand -> IO ()
keepHandlingTetrisCommands tetrisState chan = do
  cmd <- readChan chan
  newState <- handleTetrisCommand tetrisState cmd
  keepHandlingTetrisCommands newState chan

handleTetrisCommand :: TetrisState -> TetrisCommand -> IO TetrisState
handleTetrisCommand tetrisState CmdRotate = do
  let
    arena = getArenaFromState tetrisState
    newArena = rotateCurrentFigureClockwise arena
  printArena 20 20 newArena
  return $ stateWithNewArena tetrisState newArena
handleTetrisCommand tetrisState CmdDescend = do
  let
    arena = getArenaFromState tetrisState
    (newArena, _) = descendCurrentFigure arena
  printArena 20 20 newArena
  return $ stateWithNewArena tetrisState newArena
handleTetrisCommand tetrisState CmdRight = do
  let
    arena = getArenaFromState tetrisState
    newArena = moveCurrentFigureRight arena
  printArena 20 20 newArena
  return $ stateWithNewArena tetrisState newArena
handleTetrisCommand tetrisState CmdLeft = do
  let
    arena = getArenaFromState tetrisState
    newArena = moveCurrentFigureLeft arena
  printArena 20 20 newArena
  return $ stateWithNewArena tetrisState newArena
handleTetrisCommand (TetrisStateRunning arena level chan timer) CmdTick = do
  let (newArena, descended) = descendCurrentFigure arena
  if descended
  then do
    printArena 20 20 newArena
    return $ TetrisStateRunning newArena level chan timer
  else do
    let
      fixedFigureArena = fixCurrentFigure newArena
      (newFigure, newLevel) = nextFigure level
      (newFigureArena, newFigureWasSet) = setFigureIfPossible newFigure fixedFigureArena
    if newFigureWasSet
    then
      if hasFullRows newFigureArena
        then do
          printArena 20 20 fixedFigureArena
          stopTimer timer
          let noFullRowsArena = removeFullRows newFigureArena
          oneShotTimer (writeChan chan CmdPauseOrResume) (msDelay 500)
          return $ TetrisStatePaused noFullRowsArena newLevel chan
        else do
          printArena 20 20 newFigureArena
          return $ TetrisStateRunning newFigureArena newLevel chan timer
    else do
      stopTimer timer
      return $ TetrisStateRunning newFigureArena newLevel chan timer
handleTetrisCommand (TetrisStateRunning arena level chan timer) CmdPauseOrResume = do
  stopTimer timer
  return $ TetrisStatePaused arena level chan
handleTetrisCommand (TetrisStatePaused arena level chan) CmdPauseOrResume = do
  timer <- repeatedTimer (timerTick chan) (msDelay 300)
  printArena 20 20 arena
  return $ TetrisStateRunning arena level chan timer

keys2commands :: Chan TetrisCommand -> IO ()
keys2commands chan = do
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> do -- up
        writeChan chan CmdRotate
        keys2commands chan
      "\ESC[B" -> do -- down
        writeChan chan CmdDescend
        keys2commands chan
      "\ESC[C" -> do -- right
        writeChan chan CmdRight
        keys2commands chan
      "\ESC[D" -> do -- left
        writeChan chan CmdLeft
        keys2commands chan
      "\n" -> do -- enter
        writeChan chan CmdPauseOrResume
        keys2commands chan
      _ -> return ()

--------------------------------------------------------------------------------

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
