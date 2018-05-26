module EinStein.Graphics(drawBoard) where

import Graphics.Gloss

import EinStein.Types

window :: Display
window = InWindow "EinStein" (700, 700) (0, 0)  -- q&a co robi trzeci argument? / chyba nie ważne?

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

drawStone :: Player -> Dice -> EinStein.Types.Point -> Picture
drawStone player (Dice n) (Point x y) = do
    let colour = if player == Player 0 then yellow else green
    Pictures [
        translate (fromIntegral (x - 2) * 100) (fromIntegral (2 - y) * 100) $
            color colour $ circleSolid 30,
        translate (fromIntegral (x - 2) * 100) (fromIntegral (2- y) * 100) $
            scale 0.3 0.3 $ translate (-35) (-40) $ Text $ show n
     ]

drawBoard :: GameState -> IO ()
drawBoard (GameState _ _ stones) = do
    let grid = drawGrid
    let stonePic = Pictures $ map (\(p, d, f) -> drawStone p d f) stones
    display window background (Pictures [grid, stonePic])