module Animation.Common where

import System.Random (randomR,mkStdGen, getStdRandom, randomRIO)
import Control.Monad.Trans.Class (lift)


genRandom :: Int -> Int -> Int -> Int
genRandom from to seed = let gen = mkStdGen seed
              in fst $ randomR (from, to) gen

genRandomIO :: Int -> Int -> IO Int
genRandomIO from to = do
                        rnd <- randomRIO (from, to)
                        return rnd


data PlayerAction = ArrowLeft
                  | ArrowRight
                  | Pause
                  | Start
                  | Stop

instance Show PlayerAction where
  show pa = case pa of
              ArrowLeft    -> "Left"
              ArrowRight   -> "Right"
              Pause        -> "Pause"
              Start        -> "Start"
              Stop         -> "Stop"      