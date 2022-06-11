module Animation.Env where

import Animation.Common
import Data.IORef

data Env = Env {
      size :: (Int, Int), -- (Width, Height)
      envVelocity :: Int,
      envPlayerLives :: Int,
      userInputRef :: IORef [PlayerAction]
    }

defaultEnv :: Env
defaultEnv = Env (40, 20) 1 3 undefined
