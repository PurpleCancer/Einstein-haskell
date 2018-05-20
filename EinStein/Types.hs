module EinStein.Types
( Winner(..)
, Point(..)
, Move(..)
, Moves(..)
, GameState(..)
) where

data Winner = Winner (Maybe Int) deriving (Show)

data Point = Point Int Int deriving (Show, Eq)

data Move = Move Int Point Point deriving (Show, Eq)

data Moves = Moves [Move] deriving (Show)

data GameState = GameState Int Int [(Int, Point)]