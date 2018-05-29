module EinStein.Game(playEinstein) where

import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Control.Exception

import EinStein.Graphics
import EinStein.Logic
import EinStein.Types

handleEvent :: Event -> GameSnapshot -> GameSnapshot
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y))
  (GameSnapshot selected state) = do
    let fX = div (round $ x + 250.0) 100
    let fY = 4 - (div (round $ y + 250.0) 100)

    let selectedStone = stoneAt state $ Point fX fY
    let (GameState player dice stones) = state
    
    case selected of
      Just stone -> if verifyLegalMove state (Move player
                                                   (stone2Point stone)
                                                   (Point fX fY))
                    then
                -- OK! Now I’m at wit’s end
                        doMove state (Move player (stone2Point stone)
                                          (Point fX fY)) >>=
                        \newState -> (GameSnapshot Nothing newState)
                else (GameSnapshot selected state)
      Nothing -> if verifyLegalSelect state selectedStone
                    then (GameSnapshot selectedStone state)
                    else (GameSnapshot selected state)
handleEvent _ state = state

playEinstein :: IO ()
playEinstein = startState >>=
    \state -> startGame (GameSnapshot Nothing state) handleEvent
