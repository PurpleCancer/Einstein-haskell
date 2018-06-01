module EinStein.Game(playEinstein) where

import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Control.Exception

import EinStein.Graphics
import EinStein.Logic
import EinStein.Types

moveIfPossible :: GameState -> Move -> [Int] -> Maybe Stone -> Game
moveIfPossible state move diceList selected = do
             if verifyLegalMove state  move
                then Game Nothing (doMove' state move (Dice $ head diceList))
                          (tail diceList)
             else Game selected state diceList


handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y))
  (Game selected state diceList) = do
    let fX = div (round $ x + 250.0) 100
    let fY = 4 - (div (round $ y + 250.0) 100)

    let selectedStone = stoneAt state $ Point fX fY
    let (GameState player dice stones) = state
    
    case selected of
      Just stone ->
          case selectedStone of
            Nothing -> moveIfPossible state (Move player
                                                  (stone2Point stone)
                                                  (Point fX fY))
                                      diceList selected
            Just s ->
                if stone == s
                   then Game Nothing state diceList
                else
                   moveIfPossible state (Move player
                                                  (stone2Point stone)
                                                  (Point fX fY))
                                      diceList selected
      Nothing -> if verifyLegalSelect state selectedStone
                    then Game selectedStone state diceList
                    else Game selected state diceList
handleEvent _ state = state

playEinstein :: IO ()
playEinstein = do 
    state <- startState
    listOfDice <- randomList 512
    startGame (Game Nothing state listOfDice) handleEvent
