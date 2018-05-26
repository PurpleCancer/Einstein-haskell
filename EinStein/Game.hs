module EinStein.Game(playEinstein) where

import Graphics.Gloss.Interface.Pure.Game

import EinStein.Graphics
import EinStein.Logic
import EinStein.Types

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    -- todo
    -- calculate clicked field
    -- if selectedStone (I need this in GameState) is Nothing
    -- then if legal(clickedField, dice)
    --     then state{selectedStone = clickedField}
    --     else state = state
    -- else if legal(move(selectedStone, clickedField))
    --     then doMove
    --     else state = state
    state
handleEvent _ state = state

playEinstein :: IO ()
playEinstein = do
    start <- startState  -- q&a czy nie można w jednej linii? < i v
    startGame start handleEvent
    -- drawBoard start
