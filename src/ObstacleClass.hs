module ObstacleClass(
    floorPosition,
    ceilPosition,
    topWallFindPosition,
    downWallFindPosition,
    wallDownPosition,
    wallRightPosition,
    wallLeftPosition,
    isPass
) where

import SizeSetting
import Control.Monad.State


floorPosition :: (Double, Double)
floorPosition = ((fst middlePosition), (snd floorSize)/2)

ceilPosition :: (Double, Double)
ceilPosition = ((fst middlePosition), (fromIntegral((snd windowSize))-((snd floorSize)/2)))

topWallFindPosition :: Double -> Double -> Double
topWallFindPosition center hole = (center + ((snd wallSize)/2) + (hole/2))

downWallFindPosition :: Double -> Double -> Double
downWallFindPosition center hole = (center - ((snd wallSize)/2) - (hole/2))

wallDownPosition :: Double -> Double
--wallDownPosition = \y -> y - 615
wallDownPosition = \y -> -50

wallRightPosition :: Double
wallRightPosition = (1024+((fst wallSize)/2))

wallLeftPosition :: Double
wallLeftPosition = (0-((fst wallSize)/2))

isPass :: Double -> Bool
isPass position
    | position > wallLeftPosition = False
    | otherwise = True
