module SizeSetting(
    setBackgroudSize,
    setWallSize,
    setFloorOrCeilSize,
    setFlappyBirdSize,
    setWindowSize,
    setMiddleWindowSize,
    wallSize,
    floorSize,
    flappyBirdSize,
    backgroudSize,
    goldSize,
    windowSize,
    middlePosition,
) where

import Graphics.UI.Fungen

setSize :: Double -> Double -> (Double, Double)
setSize = \x y -> (x, y)

--set the size of background
setBackgroudSize :: Double -> Double -> (Double, Double)
setBackgroudSize = \x y -> (x, y)

-- set the size of wall
setWallSize :: Double -> Double -> (Double, Double)
setWallSize = \x y -> (x, y)

-- set the size of floor
setFloorOrCeilSize :: Double -> Double -> (Double, Double)
setFloorOrCeilSize = \x y -> (x, y)

-- set the size of flappy bird
setFlappyBirdSize :: Double -> Double -> (Double, Double)
setFlappyBirdSize = \x y -> (x, y)

setWindowSize :: Int -> Int -> (Int, Int)
setWindowSize = \x y -> (x, y)

setMiddleWindowSize :: (Int,Int) -> (Double, Double)
setMiddleWindowSize = \(x,y) -> (fromIntegral(x `div` 2), fromIntegral(y `div` 2))


--init the size of picture
wallSize = setWallSize 69 491
floorSize = setFloorOrCeilSize 1024 24
flappyBirdSize = setFlappyBirdSize 40 30
backgroudSize = setBackgroudSize 341 615
goldSize = setSize 20 20

--init the size of window
windowSize = setWindowSize 1024 615
middlePosition = setMiddleWindowSize windowSize
