module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import System.Random (randomR,mkStdGen,randomRIO)
import System.IO (hFlush, hReady, stdin, stdout, hSetEcho, hSetBuffering, BufferMode(..))
import Data.IORef
import Data.Char

import Animation.Env (Env(..), defaultEnv)
import Animation.Type (Animation)
import Animation.Common

data Direction
      = Positive
      | Negative
      | Neutral

directionFromInt :: Int -> Direction
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Invalid Direction"

directionToMultiplier :: Direction -> Int
directionToMultiplier Positive = 1
directionToMultiplier Negative = -1
directionToMultiplier Neutral = 0

keyToAction :: String -> PlayerAction
keyToAction chars = undefined



data GameSt = Running
            | Paused
            | Stopped
            | GameOver
            | LostBall
----------------------
-- Animation State
----------------------
data St =
  St {
        status :: GameSt
      , ball   :: BallSt
      , player :: PlayerSt
      , bricks :: [Brick]
      , items  :: [BonusItem]
  }
  --deriving Show

----------------------
-- Ball State
----------------------

data BallSt =
  BallSt { position :: (Int, Int),
           direction :: (Direction, Direction)
  }


----------------------
-- Player State
----------------------
data PlayerSt =
  PlayerSt {
      playerXposition :: Int, -- X position of the board
      playersize     :: Int, -- length of the board
      playerlives    :: Int, -- number of remaining lives      
      playerlevel    :: Int, -- current level
      playerscore    :: Int,  -- current score
      playeraction   :: Maybe PlayerAction
  }



-------------------
-- Brick State
-------------------
data Brick =
    Brick {
      brickposition :: (Int, Int), -- X, Y      
      brickstrength :: Int, -- number of hits required to destroy it
      bonusitem :: Maybe BonusItem -- item dropeed if shooted
    }

instance Show Brick where
  show bk = "Brick Position: "++show (brickposition bk)
              ++" - Strength: "++show (brickstrength bk)
              ++" - Item: "++ bi
              where bi = case bonusitem bk of
                              Nothing -> "<none>"
                              Just c -> [itemgraph c]


-------------------------
-- Bonus Item State
-------------------------
data BonusItem =
    BonusItem {
      itemposition :: (Int, Int), -- X, Y
      itemgraph   :: Char
    }

instance Show BonusItem where
    show bi = "Item: "++show (itemgraph bi) ++show (itemposition bi)

instance Show PlayerSt where
  show pst = "Remaining Lives: "++show (playerlives pst)
              ++" - Score: "++show (playerscore pst)
              ++" - Level: "++show (playerlevel pst)
              ++" - Last Move: "++show (playeraction pst)

instance Show Direction where
  show dir = case dir of
                Positive -> "Positive"
                Negative -> "Negative"
                Neutral -> "Neutral"

instance Show BallSt where
  show bst = "Position: "++show (position bst)
              ++" - Direction: "++show (direction bst)

instance Show GameSt where
  show gs = case gs of
              Running -> "Running"
              Paused -> "Paused"
              LostBall -> "Paused (LostBall)"
              Stopped -> "Stopped"
              GameOver -> "GAME OVER"



getPlayerAction :: IORef [PlayerAction] -> IO (Maybe PlayerAction)
getPlayerAction actionsref = do
                        actions <- readIORef actionsref
                        if null actions then return Nothing
                        else do
                          let action = head actions
                          writeIORef actionsref (tail actions)
                          return (Just action)

{-
Source : https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
-}
getKey :: GameSt -> IO (Maybe PlayerAction)
getKey gamestatus = do
          hSetEcho stdout False
          hSetBuffering stdin NoBuffering
          hSetEcho stdin False
          more <- hReady stdin
          if more then do
                          key <- getChar
                          case toUpper key of
                            'S' -> case gamestatus of
                                      GameOver -> return Nothing -- prevent player to press S on GameOver
                                      _ -> return (Just Start)
                            'P' -> case gamestatus of
                                      GameOver -> return Nothing -- prevent player to press S on GameOver
                                      _ -> return (Just Pause)
                            'Z' -> return (Just Stop)
                            '\ESC' -> getKey gamestatus -- read another key .. probably Arrows have been pressed
                            '[' -> getKey gamestatus -- read another key .. probably Arrows have been pressed
                            'C' -> return (Just ArrowRight)
                            'D' -> return (Just ArrowLeft)
                            _ -> return Nothing
                  else return Nothing


{-  case key of
    "\ESC[C" -> return (Just ArrowRight)
    "\ESC[D" -> return (Just ArrowLeft)
    _        -> return Nothing    
    "S"      -> return Just Start
    "P"      -> return Just Pause
-}

-- | next 
-- | This is the function called by the IO Main
-- | It manages the User Input and push it into the State using the nextInternal function (that returns the nextState)
next :: Animation Env St ()
next = do
      env <- ask               --- no lift to use the "ask" function of the ReaderT monad and retrieve the Env 
      gamestate <- lift get  --- lift 1 Level to use the "get" function of the StateT monad and retrieve the St 
      userinput <- lift $ lift $ getKey (status gamestate) --- lift 2 Levels to use IO
      let userinputref = userInputRef env
      userinputs <- lift $ lift $ readIORef userinputref
      let newinputs = userinputs ++ case userinput of
                                      Nothing -> []
                                      Just x -> [x]
      lift $ lift $ writeIORef userinputref newinputs
      playeraction <- lift $ lift $ getPlayerAction userinputref
      prevSt <- lift get
      lift (put (nextInternal env playeraction prevSt))

defaultSt :: St
defaultSt = St {status = Stopped, ball = defaultBallSt, player = defaultPlayerSt, bricks = generateBricks defaultEnv 1, items = []}

defaultBallSt :: BallSt
defaultBallSt = BallSt (0,0) (Neutral, Neutral)

defaultPlayerSt :: PlayerSt
defaultPlayerSt = PlayerSt {
                playersize     = 5,
                playerXposition = 1,
                playerlives    = 3,
                playerlevel    = 1,
                playerscore    = 0,
                playeraction   = Nothing
              }

{-
generateBrickV2 :: Env -> Int -> Int -> Int -> Int -> Brick
generateBrickV2 env posx posy strength itemtype = Brick {
                                              brickposition = (posx, posy),
                                              brickstrength = strength,
                                              bonusitem = Just BonusItem {
                                                itemposition = (posx, posy),
                                                itemgraph = '↓'
                                              }
                                          }

generateBricksV2 :: Env -> Int -> IO [Brick]
generateBricksV2 env level = do 
                          posx <- randomRIO (1, fst (size env))
                          posy <- randomRIO (1, snd (size env))
                          strength <- randomRIO (1, min 4 level)                          
                          itype <- randomRIO (1,3) {- to be implemented -}
                          return sequence $ (map (generateBrickV2 env posx posy strength) [1..20])
                            
-}

-- | generateBrick 
-- | generate a Brick using random values for Position and strength ... TO BE UDPATED
generateBrick :: Env -> Int -> Int -> Brick
generateBrick env level seed =
    let posx = genRandom 1 (fst (size env)) seed
        posy = genRandom 1 (snd (size env) - 1) seed
    in Brick {
      brickposition = (posx, posy),
      brickstrength =  genRandom 1 (min level 4) seed,
      bonusitem = Just BonusItem {
        itemposition = (posx, posy),
        itemgraph = '↓'
      }
    }


-- | Generates Bricks with a random position
-- | the number of Bricks is based on the Game Level
generateBricks :: Env -> Int -> [Brick]
generateBricks env level = map (generateBrick env level) [1..min 4 level*min (fst (size env)) (snd (size env))]

-- | Update the "brickstrength" by -1 on all bricks hit by the ball
updateHitBricks ::[Brick] -> [Brick]
updateHitBricks [] = []
updateHitBricks bricks = map (\brick -> brick { brickstrength = brickstrength brick - 1}) bricks

-- | Update BonusItems Y position to make them fall down
-- | It returns the list of the updated BonusItems removing the ones falled out of the game frame.
updateBonusItemsPosition :: [BonusItem] -> Int -> [BonusItem]
updateBonusItemsPosition [] _ = []
updateBonusItemsPosition bitems height = map (\bitem -> bitem { itemposition = (fst (itemposition bitem), snd (itemposition bitem) + 1) } ) filteredbitems
                                  where filteredbitems = filter ((\(x,y) -> y <= height) . itemposition) bitems
                            
-- | nextInternal
-- | This is the MAIN function of th game, starting from the current state it calculated the "future" state of the game and pass it back to the [next::] function
nextInternal :: Env -> Maybe PlayerAction -> St -> St
nextInternal (Env (width, height) velocity maxplayerlives uinput) paction gamestate@(St gamestatus ballstate@(BallSt (prevX, prevY) (prevXdir, prevYdir)) pstate bkstate bitems) =
      case gamestatus of
        Paused -> newgamestate
        LostBall -> newgamestate { ball = ballstate { position = (newplayerXposition + div (playersize pstate) 2, height) }
                                 , player = pstate { playerXposition = newplayerXposition }
                                 }
        Stopped -> newgamestate
        GameOver -> newgamestate
        Running -> St { status = newgamestatus
                      , ball = newballstate
                      , player = pstate { playerscore = newplayerscore
                                        , playeraction = paction
                                        , playerlives = newplayerlives
                                        , playerXposition = newplayerXposition
                                        , playersize = newplayersize
                                        }
                      , bricks = newbricks
                      , items = newbitems
                    }
      where
        newgamestate  = case paction of
                            Just Pause -> gamestate { status = Paused }
                            Just Start -> gamestate { status = Running }
                            Just Stop -> defaultSt
                            _ -> gamestate
        newgamestatus = if ballislost then if newplayerlives == 0 then GameOver else LostBall else
                                                case paction of
                                                    Just Pause -> Paused
                                                    Just Start -> Running
                                                    Just Stop ->  Stopped
                                                    _ -> gamestatus
        -- | set the newballstate (new position and direction) ... if the ball was lost the ball moves together with the player board                            
        newballstate  = BallSt {  position   = if ballislost then (newplayerXposition + div (playersize pstate) 2, height) else (newX, newY)
                               ,  direction  = (newXdir, newYdir)
                        }
        -- | set the newplayerXposition based on the user input (PlayerAction)
        newplayerXposition = case paction of
                                Just ArrowLeft -> max 1 (playerXposition pstate)-1
                                Just ArrowRight -> min (playerXposition pstate +1) (width - playersize pstate)
                                _ -> playerXposition pstate
        -- | set the newplayersize if the player get the BonusItem
        newplayersize = if not (null bonusItemHit) then 
                                                    case itemgraph (head bonusItemHit) of    
                                                          '↓' -> playersize pstate + 1
                                                          'S' -> playersize pstate - 1
                                                          _ -> playersize pstate
                        else playersize pstate
        -- | bHitbrick is true whenever the ball hit a brick
        bHitbrick = not (null (filter ((\(x,y) -> x == newX && y == newY) . brickposition) bkstate))
        -- | bHitplayer is true whenever the ball hit the player board
        bHitplayer = newY == height && newX>=playerXposition pstate && newX<=playerXposition pstate+playersize pstate
        -- | bonusItemHit contains the BonusItem that hit the player board
        bonusItemHit = filter ((\(x,y) -> y+1 == height && x>=playerXposition pstate && x<=playerXposition pstate + playersize pstate) . itemposition) bitems
        -- | hitplayernewXdir returns the new direction of the ball once it hits the playerboard
        -- |                  if the ball hit the player board in the first half, the new direction will be "Negative"
        -- |                  if the ball hit the player board in the second half, the new direction will be "Positive"
        -- |                  if the ball hit the player board in the middle, the new direction will be "Netrual"
        hitplayernewXdir
          | bHitplayer && newX-playerXposition pstate > div (playersize pstate) 2 = Positive
          | bHitplayer && newX-playerXposition pstate < div (playersize pstate) 2 = Negative
          | otherwise = Neutral          
        -- | ballislost returns true if it goes down without being hit by the player
        ballislost = newY == height && not bHitplayer
        -- | newbricks returns the remaining bricks (bricks not hit by the ball++bricks with the updated strength still > 1) 
        newbricks =  filter ((\(x,y) -> x /= newX || y /= newY) . brickposition) bkstate
                     ++
                     updateHitBricks (filter (\brick -> brickstrength brick > 1) (filter ((\(x,y) -> x == newX && y == newY) . brickposition) bkstate)) -- update the hit brick strength 
        -- | newXunbounded returns the new X position of the ball without boundaries checks
        newXunbounded = prevX + directionToMultiplier prevXdir * velocity
        -- | newYunbounded returns the new Y position of the ball without boundaries checks
        newYunbounded = prevY + directionToMultiplier prevYdir * velocity
        -- | newX returns the new X position of the ball considering the boundaries (and other obstacles in future)
        newX =
          case prevXdir of
            Neutral -> newXunbounded
            Positive -> min newXunbounded width
            Negative -> max newXunbounded 0
        -- | newY returns the new Y position of the ball considering the boundaries (and other obstacles in future)
        newY =
          case prevYdir of
            Neutral -> newYunbounded
            Positive -> min newYunbounded height
            Negative -> max newYunbounded 0
        -- | newXdir returns the new X direction of the ball considering current direction, boundaries and if the ball hits the playerboard it returns directly 
        -- | the new direction based on the hit position, if hits the brick it changes the direction accordingly
        newXdir = if bHitplayer then hitplayernewXdir else
                        case prevXdir of
                            Neutral -> if newXunbounded > width || bHitbrick then
                                        let seed = newXunbounded
                                        in directionFromInt (genRandom 1 2 seed)
                                        else prevXdir
                            Positive -> if newXunbounded > width || bHitbrick then Negative else Positive
                            Negative -> if newXunbounded < 0 || bHitbrick then Positive else Negative
        -- | newYdir returns the new Y direction of the ball considering current direction, boundaries and if the ball hit the brick it returns directly 
        -- | the new direction based on the hit position
        newYdir =
          case prevYdir of
            Neutral -> if newYunbounded > height || bHitbrick then
                          let seed = newYunbounded
                          in directionFromInt (genRandom 1 2 seed)
                        else prevYdir
            Positive -> if newYunbounded > height || bHitbrick then Negative else Positive
            Negative -> if newYunbounded < 0 || bHitbrick then Positive else Negative
        -- | set the newplayerscore, if the brick is hit the score increase by 1
        newplayerscore = if bHitbrick then playerscore pstate + 1  else playerscore pstate
        -- | set the newplayerlives, if the ball is lost the player lives are reduced by 1
        newplayerlives = if ballislost then playerlives pstate - 1 else playerlives pstate
        -- | set new state of the BonusItems available on the game screen and removing the items falled down
        newbitems = updateBonusItemsPosition bitems height ++ brickbonusitem
                      where brickbonusitem = if bHitbrick then 
                                                      case bonusitem (head (filter ((\(x,y) -> x == newX && y == newY) . brickposition) bkstate)) of
                                                          Nothing -> []
                                                          Just bi -> [bi]
                                              else []
