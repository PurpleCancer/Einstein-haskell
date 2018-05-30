module EinStein.Types
( Player(..)
, Dice(..)
, Winner(..)
, Point(..)
, Move(..)
, Moves(..)
, Stone(..)
, GameState(..)
, Game(..)
) where

data Player = Player Int deriving (Show, Eq)

data Dice = Dice Int deriving (Show, Eq, Ord)

data Winner = Winner (Maybe Player) deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq)

data Move = Move Player Point Point deriving (Show, Eq)

data Moves = Moves [Move] deriving (Show)

data Stone = Stone Player Dice Point deriving (Show, Eq)

data GameState = GameState Player Dice [Stone] deriving (Show, Eq)

data Game = Game (Maybe Stone) GameState [Int] deriving (Show, Eq)
