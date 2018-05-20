module EinStein.Logic
( startState
, getMoves
, doMove
, getWinner
) where

import EinStein.Types
import Data.List
import System.Random
import System.Random.Shuffle

-- public
-- generate a new game state with random setup
startState :: IO GameState
startState = do
    g <- newStdGen
    let randOut = randomR (1,6) g
    let dice = Dice (fst randOut)
    let diceList = [1, 2, 3, 4, 5, 6]
    g <- newStdGen
    let p0Dice = shuffle' diceList 6 g
    g <- newStdGen
    let p1Dice = shuffle' diceList 6 g
    let p0State = [(Player 0, Dice (p0Dice !! 0), Point 0 0),(Player 0, Dice (p0Dice !! 1), Point 0 1),(Player 0, Dice (p0Dice !! 2), Point 0 2),(Player 0, Dice (p0Dice !! 3), Point 1 0),(Player 0, Dice (p0Dice !! 4), Point 1 1),(Player 0, Dice (p0Dice !! 5), Point 2 0)]
    let p1State = [(Player 1, Dice (p1Dice !! 0), Point 4 4),(Player 1, Dice (p1Dice !! 1), Point 4 3),(Player 1, Dice (p1Dice !! 2), Point 4 2),(Player 1, Dice (p1Dice !! 3), Point 3 4),(Player 1, Dice (p1Dice !! 4), Point 3 3),(Player 1, Dice (p1Dice !! 5), Point 2 4)]
    return $ GameState (Player 0) (dice) (p0State ++ p1State)

-- verify whether the point is inside the board
legalPoint :: Point -> Bool
legalPoint (Point x y) = (&&) ((&&) (x >= 0) (y >= 0)) ((&&) (x < 5) (y < 5))

-- build a Move object
moveFromPoint :: Player -> Point -> Point -> Move
moveFromPoint pl start finish = Move pl start finish

-- generate all legal moves for a player and a point
genMoves :: Player -> Point -> Moves
genMoves pl (Point x y) = 
    Moves $ map (moveFromPoint pl (Point x y)) $ filter legalPoint [(Point (x + inc) y), (Point x (y + inc))]
        where inc = if (pl == Player 0) then 1 else -1

-- extract point from the stone tuple
stone2Point :: (Player, Dice, Point) -> Point
stone2Point (_, _, p) = p

-- flatten two Moves objects into one
flattenMoves :: Moves -> Moves -> Moves
flattenMoves (Moves m1) (Moves m2) = Moves (m1 ++ m2)

-- gets player moves for all the stones supplied in the GameState object
getPlayerMoves :: GameState -> Moves
getPlayerMoves (GameState pl _ stones) =
    foldl flattenMoves (Moves []) $ map (genMoves pl) $ map stone2Point $ filter (\(pls, _, _) -> pl == pls) stones

-- filter movable Dice from a list based on the rolled value
getLegalDice :: Dice -> [Dice] -> [Dice]
getLegalDice target ds =
    if exact
        then filter (\d -> d == target) ds
        else let
            lower = filter (\d -> d < target) ds
            higher = filter (\d -> d > target) ds
            lowerSort = sort lower
            higherSort = sort higher
            lowerSort' = reverse lowerSort
            higherSort' = reverse higherSort
            up = higherSort ++ lowerSort
            down = lowerSort' ++ higherSort'
            firstUp = take 1 up
            firstDown = take 1 down
        in
            filter (\d -> (||) (elem d firstUp) (elem d firstDown)) ds
    where exact = (length $ filter (\d -> d == target) ds) == 1

-- public
-- gets legal moves from the game state based on the rolled value
getMoves :: GameState -> Moves
getMoves (GameState p dice stones) =
    getPlayerMoves (GameState p dice legalStones)
    where playerStones = filter (\(pl, _, _) -> p == pl) stones
          playerDice = map (\(_, d, _) -> d) playerStones
          legalDice = getLegalDice dice playerDice
          legalStones = filter (\(_, d, _) -> elem d legalDice) playerStones

-- public
-- unimplemented
-- apply a move to the GameState object
doMove :: GameState -> Move -> GameState
doMove _ _ = GameState (Player 0) (Dice 0) []

-- public
-- unimplemented
-- get a winner if a game state has one
getWinner :: GameState -> Winner
getWinner _ = Winner Nothing