module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import System.Random (randomRIO)
import Data.IORef

import System.IO (hFlush, hReady, stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))
import Control.Monad (when)


import Animation.Env ( defaultEnv, Env(Env, userInputRef) )
import Animation.Render ( render )
import Animation.State
    ( PlayerSt(playerlives),
      BallSt(BallSt),
      St(St),
      GameSt(Stopped),
      directionFromInt,
      next,
      defaultSt,
      defaultPlayerSt,
      generateBricks )
import Animation.Type ( runAnimation, Animation )

putInitialState :: Animation Env St ()
putInitialState = do
      env@(Env (width, height) _ plives _) <- ask
      posX <- lift $ lift $ randomRIO (0,width)
      posY <- lift $ lift $ randomRIO (0,height)
      dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (0, 2)
      dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
      --bricks <- lift $ lift $ generateBricks env 2            
      lift $ put $ St Stopped (BallSt (posX, posY) (dirX, dirY)) defaultPlayerSt { playerlives = plives} (generateBricks env 2) []


animate :: Animation Env St ()
animate = do
      render
      next
      --lift $ lift $ flush      
      lift $ lift $ threadDelay 200000      
      animate

mainAnimation :: Animation Env St ()
mainAnimation = do
      putInitialState 
      animate

flush :: IO ()
flush = hFlush stdout

main :: IO ()
main =  do 
      userinput <- newIORef []
      runAnimation defaultEnv { userInputRef = userinput} defaultSt mainAnimation

