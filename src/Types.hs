module Types(
  GameAttribute(GA),
  GameState(LevelStart, Level, GameOver, Win),
  FBirdObject,
  FBirdAction
) where

import Graphics.UI.Fungen

data GameAttribute = GA Int Int Bool
data GameState = LevelStart Int | Level Int | GameOver | Win

type FBirdObject = GameObject ()
type FBirdAction a = IOGame GameAttribute () GameState () a
