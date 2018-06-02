module EinStein.Graphics(startGame) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Debug.Trace

import EinStein.Types
import EinStein.Logic

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

drawState :: Game -> Picture
drawState (Game _ (GameState _ _ stones) _) = do
    let grid = drawGrid
    let stonePic = Pictures $ map (\s -> drawStone s) stones
    Pictures [grid, stonePic]

drawHUD :: Game -> Picture
drawHUD (Game selected (GameState player (Dice dice) _) _) =
    Pictures [
        translate (-250) (300) $ scale 0.3 0.3 $
            Text $ "Dice: " ++ show dice ++ ", " ++ 
                (if player == Player 0 then "Yellow" else "Green")
                ++ " turn",
        translate (-250) (255) $ scale 0.3 0.3 $
            Text $ "Selected: " ++ case selected of
                 Nothing -> "None"
                 Just (Stone _ (Dice stone) _) -> show stone]

drawWinner :: Player -> Picture
drawWinner player = Pictures [
    translate (-250) 100 $ scale 0.5 0.5 $
        Text $ (if player == (Player 0) then "Yellow" else "Green") ++ " won",
    translate (-350) (-100) $ scale 0.5 0.5 $
        Text $ "Press Escape to exit"]

drawUI :: Game -> Picture
drawUI (Game selected state diceList) =
    case getWinner state of
      Winner Nothing -> Pictures [drawState (Game selected state diceList),
                                  drawHUD (Game selected state diceList)]
      Winner (Just winner) -> drawWinner winner

update :: Float -> Game -> Game
update _ snapshot = snapshot

startGame :: Game -> (Event -> Game -> Game) -> IO ()
startGame start handleEvent = play window background 60 start drawUI
    handleEvent update
