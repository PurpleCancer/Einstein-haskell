module EinStein.Game(playEinstein) where

import EinStein.Graphics
import EinStein.Logic

playEinstein :: IO ()
playEinstein = do
    start <- startState  -- q&a czy nie można w jednej linii?
    drawBoard start
