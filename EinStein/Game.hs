module EinStein.Game(playEinstein) where

import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Control.Exception

import EinStein.Graphics
import EinStein.Logic
import EinStein.Types

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y))
  (Game selected state diceList) = do
    let fX = div (round $ x + 250.0) 100
    let fY = 4 - (div (round $ y + 250.0) 100)

    let selectedStone = stoneAt state $ Point fX fY
    let (GameState player dice stones) = state
    
    -- todo if selected == selected stone -> deselect
    case selected of
      Just stone -> if verifyLegalMove state (Move player
                                                   (stone2Point stone)
                                                   (Point fX fY))
                    then Game Nothing (doMove' state
                                               (Move player (stone2Point stone)
                                                     (Point fX fY))
                                               (Dice $ head diceList))
                              (tail diceList)
                    else Game selected state diceList
      Nothing -> if verifyLegalSelect state selectedStone
                    then Game selectedStone state diceList
                    else Game selected state diceList
handleEvent _ state = state

playEinstein :: IO ()
playEinstein = do 
    state <- startState
    listOfDice <- randomList 512
    startGame (Game Nothing state listOfDice) handleEvent
