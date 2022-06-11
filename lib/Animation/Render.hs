module Animation.Render where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)

import Animation.Env (Env(..))
import Animation.State (St(..), BallSt(..), PlayerSt(..), Brick(..), GameSt(..), BonusItem(..))
import Animation.Type (Animation)
import Animation.Common

import Data.Char (chr)

render :: Animation Env St ()
render = do
  box <- renderVal
  lift $ lift $ clearScreen
  lift (lift (putStrLn box))

renderVal :: Animation Env St String
renderVal = do
  env <- ask
  currentstate <- lift get
  return (renderInternal env currentstate)

-- | renderInternal 
-- | This method returns the Game Board rendering the various part of the screen
renderInternal :: Env -> St -> String
renderInternal env st = showStats env st
                  ++ show (items st)
                  ++ "\n"
                  ++ makeBox (size env) st
                  ++ renderPlayerBoard c (playerXposition (player st)) (playersize (player st))
                  ++ renderCommands (status st)
                where c = (fst (size env))

-- | makeBox
-- | This method returns the Game Box --- rendering Row by Row and Column by Column all the items... including Bricks, Ball and Bonus Items
makeBox :: (Int, Int) -> St -> String
makeBox (c, r) gamestats =  unlines (
         [(makeLines '-' '-' c Nothing)] ++ renderRows
      )
      where
        renderRows = let rows = [0..r]
                  in map (renderRow gamestats) rows
        renderRow :: St -> Int -> String
        renderRow gs r = let cols = [(-1)..(c+1)]
                  in map (renderCol gs r) cols
        renderBrick bk = case brickstrength bk of
                              1 -> '-'
                              2 -> '≈'
                              3 -> '≡'
                              _ -> '#'
        renderBonus bi = itemgraph  bi
        renderCol gs rr cc =
                  let bks = filter ((\(x,y) -> x == cc && y == rr) . brickposition) (bricks gs)
                      bitems = filter ((\(x,y) -> x == cc && y == rr) . itemposition) (items gs)
                      (bx, by) = position (ball gs)
                      (wx, wy) = if null bks then (-1,-1) else brickposition (head bks)
                      (ix, iy) = if null bitems then (-1,-1) else itemposition (head bitems)
                      ballorwall
                        | (cc == (-1) || cc == (c+1)) = '│' -- frame
                        | (bx == cc && by == rr) = 'o' -- ball
                        | (wx == cc && wy == rr) = renderBrick (head bks) -- wall
                        | (ix == cc && iy == rr) = renderBonus (head bitems) -- BonusItem
                        | otherwise = ' ' -- screen
                  in ballorwall

-- | makeLines
-- | This method renders a line in the Game Screen (actually it is just for the top line and game stats) .. TO BE UPDATED ... 
makeLines :: Char -> Char -> Int -> Maybe Int -> String
makeLines endChar innerChar cols mb =
  case mb of
    Nothing -> [endChar]++(replicate cols innerChar)++[endChar]
    Just ballc ->
        let bM = min ballc (cols - 1)
          in
            [endChar]
            ++ replicate ballc innerChar
            ++ ['o']
            ++ replicate (cols-bM-1) innerChar
            ++ [endChar]

-- | showStats
-- | This method shows game information on top of the Game Board ... it uses the Show implementations of the various Data Types
showStats :: Env -> St -> String
showStats env st =
            makeLines '-' '-' 50 Nothing ++ "\n"
          ++ show (ball st)++ "\n"
          ++ "Status: " ++ show (status st) ++ " - "
          ++ show (player st) ++ "\n"
          ++ makeLines '-' '-' 50 Nothing ++ "\n"

-- | clearScreen
-- | This function printed to the IO clear the screen
clearScreen ::IO ()
clearScreen = putStr "\ESC[2J"

-- | renderPlayerBoard framewidth -> posx -> width -> String
-- | This method renders the Player board
renderPlayerBoard :: Int -> Int -> Int -> String
renderPlayerBoard cols x wd =
      let posx = min x (cols - wd)
          in
            ['│']
            ++ replicate posx ' '
            ++ "╞" ++ replicate (wd-2) '═' ++"╡"
            ++ replicate (cols-wd-posx+1) ' '
            ++ "│\n\n"

-- | renderCommands
-- | This method renders the user options below the Game board
-- | the options might change based on the current Game State
renderCommands :: GameSt -> String
renderCommands gstate = case gstate of
                          Running -> "[P]-Pause [Z]-Stop [Left/Right]-Move\n"
                          Stopped -> "[S]-Start\n"
                          GameOver -> "GAME OVER press Z to Start again\n"
                          Paused  -> "[S]-Start [Z]-Stop\n"
                          LostBall  -> "[S]-Start [Z]-Stop\n"
