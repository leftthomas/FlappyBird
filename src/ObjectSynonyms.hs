module ObjectSynonyms(
    GameAttribute(GameAttribute),
    GameState(LevelStart,Level,GameOver,Win),
    FlappyBird,
    IOGame',
    Wall,
    Floor,
    Gold
)
where

import Graphics.UI.Fungen
-- according to Fungen, we need to use GameObject and IOGame
--newtype IOGame t s u v a = IOG (Game  t s u v -> IO (Game t s u v,a))
---- * t - /T/op-level game attribute type,
  --
  -- * s - /S/prite object attribute type,
  --
  -- * u - /U/pdating game state type,
  --
  -- * v - /V/icinity (map tile) attribute type.
data GameAttribute = GameAttribute Int Double Int Bool Int Int
data GameState = LevelStart Int | Level Int | GameOver | Win
type FlappyBird  = GameObject ()
type Wall  = GameObject ()
type Floor = GameObject ()
type Gold = GameObject ()
type IOGame' a = IOGame GameAttribute () GameState () a


