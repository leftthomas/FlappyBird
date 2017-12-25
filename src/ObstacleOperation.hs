module ObstacleOperation(
    wallSpeed,
    createFloorAndCeil,
    createWalls,
    setObjectsAsleep,
    stopMovingWalls,
    setWallsPosition,
    computeScore,
    obstacleCycle,
) where


import Graphics.UI.Fungen
import ObstacleClass
import Picture
import SizeSetting
import ObjectSynonyms
import System.IO.Unsafe
import System.Random

wallSpeed = -10
wallHole = 125.0

createFloorAndCeil :: [Floor]
createFloorAndCeil =
  [object "floor" floor False floorPosition (0, 0) (), object "ceil" ceil False ceilPosition (0, 0) ()]
  where
    floor = Tex floorSize (getPictureIndex "floor" pictures)
    ceil = Tex floorSize (getPictureIndex "ceil" pictures)

createWalls :: [Wall]
createWalls =
  [ object "topWall_0" wall False topWallPosition_0 (wallSpeed, 0) ()
  , object "downWall_0" wall False downWallPosition_0 (wallSpeed, 0) ()
  , object "topWall_1" wall False topWallPosition_1 (wallSpeed, 0) ()
  , object "downWall_1" wall False downWallPosition_1 (wallSpeed, 0) ()
  , object "topWall_2" wall False topWallPosition_2 (wallSpeed, 0) ()
  , object "downWall_2" wall False downWallPosition_2 (wallSpeed, 0) ()
  ]
  where
    wall = Tex wallSize (getPictureIndex "wall" pictures)
    randomY = [unsafePerformIO $ randomRIO (200, 500 :: Int) | x <- [1 .. 10]]
    y_0 = fromIntegral (head randomY)
    topWallPosition_0 = (0, topWallFindPosition y_0 wallHole)
    downWallPosition_0 = (0, downWallFindPosition y_0 wallHole)
    y_1 = fromIntegral (randomY !! 1)
    topWallPosition_1 = (0, topWallFindPosition y_1 wallHole)
    downWallPosition_1 = (0, downWallFindPosition y_1 wallHole)
    y_2 = fromIntegral (randomY !! 2)
    topWallPosition_2 = (0, topWallFindPosition y_2 wallHole)
    downWallPosition_2 = (0, downWallFindPosition y_2 wallHole)


setObjectsAsleep :: [GameObject ()] -> Bool -> Int -> IOGame' ()
setObjectsAsleep _ _ 0  = return ()
setObjectsAsleep [] _ _ = return ()
setObjectsAsleep (x:xs) asleep n = do
  setObjectAsleep asleep x
  setObjectsAsleep xs asleep (n - 1)


stopMovingWalls :: [Wall]  -> IOGame' ()
stopMovingWalls [] = return()
stopMovingWalls (x:xs)  = do
  setObjectSpeed (0,0) x
  stopMovingWalls xs


setWallsPosition :: [Wall] -> Int -> IOGame' ()
setWallsPosition [] _ = return()
setWallsPosition _ 0 = return()
setWallsPosition (x:xs) n = do
  (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
  (_, y) <- getObjectPosition x
  setObjectPosition (wallRightPosition + wallSpace, y) x
  if(down == False) then do
     setGameAttribute (GameAttribute score wallSpace goldNumber True tempY lastCollisionGold)
     setWallsPosition xs (n - 1)
  else do
     setGameAttribute (GameAttribute score (wallSpace + 350) goldNumber False tempY lastCollisionGold)
     setWallsPosition xs (n - 1)


computeScore :: [Wall] -> Int -> IOGame' ()
computeScore _ 0 = return()
computeScore [] _ = return()
computeScore (x:xs) n = do
  (px, py) <- getObjectPosition x
  if (isPass px) then (do
    (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
    let newY = (take 1 $ randomRs (200,500) (mkStdGen tempY) :: [Int])!!0
    if(down == False) then do
      setObjectPosition (wallRightPosition, (topWallFindPosition (fromIntegral(newY)) wallHole)) x
      setGameAttribute (GameAttribute (score+1) wallSpace goldNumber True tempY lastCollisionGold)
    else do
      setObjectPosition (wallRightPosition, (downWallFindPosition (fromIntegral(newY)) wallHole)) x
      setGameAttribute (GameAttribute score wallSpace goldNumber False newY lastCollisionGold)
                      )
  else do
      (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
      setGameAttribute (GameAttribute score wallSpace goldNumber False tempY lastCollisionGold)
  computeScore xs (n - 1)


obstacleCycle :: IOGame' ()
obstacleCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do
      walls <- getObjectsFromGroup "walls"
      setObjectsAsleep walls True (2 * n)
      (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
      setGameAttribute (GameAttribute score 0 goldNumber down tempY lastCollisionGold)
      setWallsPosition walls (2 * n)
    Level n -> do
      walls <- getObjectsFromGroup "walls"
      setObjectsAsleep walls False (2 * n)
      computeScore walls (2 * n)
    GameOver -> return ()
    Win -> do
      walls <- getObjectsFromGroup "walls"
      setObjectsAsleep walls True 6