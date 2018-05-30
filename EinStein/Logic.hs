module EinStein.Logic
( startState
, getMoves
, doMove'
, getWinner
, stone2Point
, verifyLegalMove
, verifyLegalSelect
, stoneAt
, randomList
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
    let p0State = [Stone (Player 0) (Dice (p0Dice !! 0)) (Point 0 0),Stone (Player 0) (Dice (p0Dice !! 1)) (Point 0 1),Stone (Player 0) (Dice (p0Dice !! 2)) (Point 0 2),Stone (Player 0) (Dice (p0Dice !! 3)) (Point 1 0),Stone (Player 0) (Dice (p0Dice !! 4)) (Point 1 1),Stone (Player 0) (Dice (p0Dice !! 5)) (Point 2 0)]
    let p1State = [Stone (Player 1) (Dice (p1Dice !! 0)) (Point 4 4),Stone (Player 1) (Dice (p1Dice !! 1)) (Point 4 3),Stone (Player 1) (Dice (p1Dice !! 2)) (Point 4 2),Stone (Player 1) (Dice (p1Dice !! 3)) (Point 3 4),Stone (Player 1) (Dice (p1Dice !! 4)) (Point 3 3),Stone (Player 1) (Dice (p1Dice !! 5)) (Point 2 4)]
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
    Moves $ map (moveFromPoint pl (Point x y)) $ filter legalPoint [(Point (x + inc) y), (Point x (y + inc)), (Point (x + inc) (y + inc))]
        where inc = if (pl == Player 0) then 1 else -1

-- extract point from the stone object
stone2Point :: Stone -> Point
stone2Point (Stone _ _ p) = p

-- flatten two Moves objects into one
flattenMoves :: Moves -> Moves -> Moves
flattenMoves (Moves m1) (Moves m2) = Moves (m1 ++ m2)

-- gets player moves for all the stones supplied in the GameState object
getPlayerMoves :: GameState -> Moves
getPlayerMoves (GameState pl _ stones) =
    foldl flattenMoves (Moves []) $ map (genMoves pl) $ map stone2Point $ filter (\(Stone pls _ _) -> pl == pls) stones

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
    where playerStones = filter (\(Stone pl _ _) -> p == pl) stones
          playerDice = map (\(Stone _ d _) -> d) playerStones
          legalDice = getLegalDice dice playerDice
          legalStones = filter (\(Stone _ d _) -> elem d legalDice) playerStones



-- verify whether the supplied move is legal in the current game state
verifyLegalMove :: GameState -> Move -> Bool
verifyLegalMove (GameState player dice stones) (Move mplayer p1 p2) =
    (&&) (player == mplayer)
    $ elem (Move mplayer p1 p2) moves
    where
        (Moves moves) = getMoves (GameState player dice stones)

-- apply a move to the GameState object with supplying a new rolled dice value
doMove' :: GameState -> Move -> Dice -> GameState
doMove' (GameState (Player pID) dice stones) move newDice =
    if verifyLegalMove (GameState (Player pID) dice stones) move
    then let
        Move _ oldPoint newPoint = move
        oldStone = head $ filter (\(Stone _ _ p) -> p == oldPoint) stones
        Stone player diceVal _ = oldStone
        newStone = Stone player diceVal newPoint
        newStones = (filter (\(Stone _ _ p) -> (&&) (p /= oldPoint) (p /= newPoint)) stones) ++ [newStone]
        newPID = mod (pID + 1) 2
    in (GameState (Player newPID) newDice newStones)
    else (GameState (Player pID) dice stones)

-- public
-- apply a move to the GameState object
doMove :: GameState -> Move -> IO GameState
doMove gs move = do
    g <- newStdGen
    let randOut = randomR (1,6) g
    let dice = Dice (fst randOut)
    return $ doMove' gs move dice

-- public
-- get a winner if a game state has one
getWinner :: GameState -> Winner
getWinner (GameState player _ stones) =
    let p0Winners = filter (\(Stone pl _ p) -> (&&) (pl == (Player 0)) (p == (Point 4 4))) stones
        p1Winners = filter (\(Stone pl _ p) -> (&&) (pl == (Player 1)) (p == (Point 0 0))) stones
        p0Stones = filter (\(Stone pl _ _) -> pl == (Player 0)) stones
        p1Stones = filter (\(Stone pl _ _) -> pl == (Player 1)) stones
    in if ((length p0Winners) > 0 || (length p1Stones) == 0) then Winner (Just (Player 0))
    else if ((length p1Winners) > 0 || (length p0Stones) == 0) then Winner (Just (Player 1))
    else Winner Nothing

-- public
-- check if selection is valid
verifyLegalSelect :: GameState -> Maybe Stone -> Bool
verifyLegalSelect (GameState gamePlayer dice stones) selection = 
    case selection of
      Nothing -> False
      Just stone -> do
          let (Stone player number _) = stone
          let (Stone _ bigger _) =
                  minimumBy (\(Stone _ n1 _) -> \(Stone _ n2 _) ->
                      compare n1 n2)
                  (filter (\(Stone pl num _) -> (pl == player) &&
                      (num >= dice)) stones)
          let (Stone _ smaller _) =
                  maximumBy (\(Stone _ n1 _) -> \(Stone _ n2 _) ->
                      compare n1 n2)
                  (filter (\(Stone pl num _) -> (pl == player) &&
                      (num <= dice)) stones)
          (number == smaller || number == bigger) && (gamePlayer == player)

-- public
-- get stone on field (or Nothing)
stoneAt :: GameState -> EinStein.Types.Point -> Maybe Stone
stoneAt (GameState _ _ stones) field =
    case filter (\(Stone _ _ stoneField) -> stoneField == field) stones of
      [] -> Nothing
      stones -> Just (stones !! 0)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 
