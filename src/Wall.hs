module Wall(
  wallFindUpPosition,
  wallFindDownPosition,
  wallOffScreen,
  wallRightmostPosition, wallLeftmostPosition,
  floorBottomPosition, floorUpperPosition
) where

import Textures
import Variables

wallFindUpPosition :: Int -> Int -> Double
wallFindUpPosition center hole = fromIntegral (center + ((round (snd textureWallSize)`div`2)) + (hole`div`2))

wallFindDownPosition :: Int -> Int -> Double
wallFindDownPosition center hole = fromIntegral (center - ((round (snd textureWallSize))`div`2) - (hole`div`2))

wallRightmostPosition :: Double
wallRightmostPosition = (1024+((fst textureWallSize)/2))

wallLeftmostPosition :: Double
wallLeftmostPosition = (0-((fst textureWallSize)/2))

wallOffScreen :: Double -> Bool
wallOffScreen position | position > wallLeftmostPosition = False
                       | otherwise = True

floorBottomPosition :: (Double, Double)
floorBottomPosition = ((fst middleScreen), (snd textureFloorSize)/2)

floorUpperPosition :: (Double, Double)
floorUpperPosition = ((fst middleScreen), (fromIntegral((snd windowResolution))-((snd textureFloorSize)/2)))
