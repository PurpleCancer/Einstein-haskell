module EinStein.Graphics(startGame) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Debug.Trace

import EinStein.Types

window :: Display
window = InWindow "EinStein" (700, 700) (0, 0)

background :: Color
background = white

drawGrid :: Picture
drawGrid = translate (-250) (-250) $ Pictures [
        translate 250 250 $ color white $ rectangleSolid 500 500,
        Pictures $ map (\x -> Line [(100 * x , 0), (100 * x, 500)])
            (take 4 [1..]),
        Pictures $ map (\x -> Line [(0, 100 * x), (500, 100 * x)])
            (take 4 [1..]),
        translate 250 250 $ rectangleWire 500 500
    ]

drawStone :: Stone -> Picture
drawStone (Stone player (Dice number) (Point x y)) = do
    let colour = if player == Player 0 then yellow else green
    Pictures [
        translate (fromIntegral (x - 2) * 100) (fromIntegral (2 - y) * 100) $
            color colour $ circleSolid 30,
        translate (fromIntegral (x - 2) * 100) (fromIntegral (2- y) * 100) $
            scale 0.3 0.3 $ translate (-35) (-40) $ Text $ show number
     ]

drawState :: GameSnapshot -> Picture
drawState (GameSnapshot selected (GameState player dice stones)) = do
    let grid = drawGrid
    -- todo if selectedStone is Nothing
    -- then
    --     draw $ getHints for selecting stones
    -- else
    --     draw $ getHints for moves
    -- (I need 2 getHints. What do I have now?)
    let stonePic = Pictures $ map (\s -> drawStone s) stones
    Pictures [grid, stonePic]

update :: Float -> GameSnapshot -> GameSnapshot
update _ snapshot = snapshot

startGame :: GameSnapshot -> (Event -> GameSnapshot -> GameSnapshot) -> IO ()
startGame start handleEvent = play window background 60 start drawState
    handleEvent update >> putStrLn "out"
    -- fixme closes window on ESC but doesn’t terminate
