module Variables (
  windowResolution,
  gravity, middleScreen
) where

import Graphics.UI.Fungen
import Textures
import Types

windowResolution :: (Int, Int)
windowResolution = (1024, 615)

middleScreen :: (Double, Double)
middleScreen = (fromIntegral((fst windowResolution)`div`2), fromIntegral((fst windowResolution)`div`2))

gravity :: Double
gravity = 1.0
