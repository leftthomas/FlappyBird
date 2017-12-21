module ObstacleOperation(
    wallSpeed,
    createFloorAndCeil,
    createWalls,
    setObjectsAsleep,
    stopMovingWalls,
    setWallsPosition,
    computeScore,
    obstacleCycle
) where


import Graphics.UI.Fungen
import ObstacleClass
import Picture
import SizeSetting
import ObjectSynonyms
import System.IO.Unsafe
import System.Random

wallSpeed = -10
wallHole = 200.0

createFloorAndCeil :: [Floor]
createFloorAndCeil = [(object "floor" floor False floorPosition (0,0) ()),
                      (object "ceil" ceil False ceilPosition (0,0) ())]
                      where floor = Tex floorSize (getPictureIndex "floor" pictures)
                            ceil = Tex floorSize (getPictureIndex "ceil" pictures)

createWalls :: [Wall]
createWalls = [(object "topWall_0" wall False topWallPosition_0 (wallSpeed,0) ()),
               (object "downWall_0" wall False downWallPosition_0 (wallSpeed,0) ()),
               (object "topWall_1" wall False topWallPosition_1 (wallSpeed,0) ()),
               (object "downWall_1" wall False downWallPosition_1 (wallSpeed,0) ()),
               (object "topWall_2" wall False topWallPosition_2 (wallSpeed,0) ()),
               (object "downWall_2" wall False downWallPosition_2 (wallSpeed,0) ()),
               (object "topWall_3" wall False topWallPosition_3 (wallSpeed,0) ()),
               (object "downWall_3" wall False downWallPosition_3 (wallSpeed,0) ())]
                      where wall = Tex wallSize (getPictureIndex "wall" pictures)
                            y = fromIntegral (unsafePerformIO $ randomRIO (200, 400)::Int)
                            x = fromIntegral (unsafePerformIO $ randomRIO (100, 200)::Int)
                            randomY = [unsafePerformIO $ randomRIO (200, 500::Int) | x <- [1..10]]
                            randomX = [unsafePerformIO $ randomRIO (100, 200::Int) | x <- [1..10]]
                            y_0 = fromIntegral(randomY !! 0)
                            x_0 = wallRightPosition
                            topWallPosition_0 = (x_0, (topWallFindPosition y_0 wallHole))
                            downWallPosition_0 = (x_0, (downWallFindPosition y_0 wallHole))
                            y_1 = fromIntegral(randomY !! 1)
                            x_1 = fromIntegral(randomX !! 1)
                            topWallPosition_1 = ((wallRightPosition  + x_1), (topWallFindPosition y_1 wallHole))
                            downWallPosition_1 = ((wallRightPosition + x_1), (downWallFindPosition y_1 wallHole))
                            y_2 = fromIntegral(randomY !! 2)
                            x_2 = fromIntegral(randomX !! 2)
                            topWallPosition_2 = ((wallRightPosition + x_2), (topWallFindPosition y_2 wallHole))
                            downWallPosition_2 = ((wallRightPosition + x_2), (downWallFindPosition y_2 wallHole))
                            y_3 = fromIntegral(randomY !! 3)
                            x_3 = fromIntegral(randomX !! 3)
                            topWallPosition_3 = ((wallRightPosition + x_3), (topWallFindPosition y_3 wallHole))
                            downWallPosition_3 = ((wallRightPosition + x_3), (downWallFindPosition y_3 wallHole))

setObjectsAsleep :: [GameObject ()] -> Bool -> IOGame' ()
setObjectsAsleep [] _  = return ()
setObjectsAsleep (x:xs) asleep = do
  setObjectAsleep asleep x
  setObjectsAsleep xs asleep


stopMovingWalls :: [Wall]  -> IOGame' ()
stopMovingWalls [] = return()
stopMovingWalls (x:xs) = do
  setObjectSpeed (0,0) x
  stopMovingWalls xs


setWallsPosition :: [Wall] -> IOGame' ()
setWallsPosition []= return()
setWallsPosition (x:xs) = do
  (_, y) <- getObjectPosition x
  setObjectPosition (wallRightPosition, y) x
  setWallsPosition xs


computeScore :: [GameObject ()] -> IOGame' ()
computeScore [] = return()
computeScore (x:xs) = do
  (px, py) <- getObjectPosition x
  when (isPass px) (do
    (GameAttribute score lastWallY goldNumber open lastCollisionGold) <- getGameAttribute
    let y = fromIntegral (unsafePerformIO $ randomRIO (200, 400)::Int)
    --let randomx = fromIntegral (unsafePerformIO $ randomRIO (100, 200)::Int)
    let newTopWallPosition = (wallRightPosition, (topWallFindPosition y wallHole))
    let newDownWallPosition = (wallRightPosition, (downWallFindPosition y wallHole))
    if(open == False) then do
      setObjectPosition newTopWallPosition x
      --setObjectPosition newDownWallPosition (head xs)
      setGameAttribute (GameAttribute (score+1) lastWallY goldNumber True lastCollisionGold)
    else do
      setObjectPosition newDownWallPosition x
      setGameAttribute (GameAttribute score lastWallY goldNumber False lastCollisionGold)
                      )
  computeScore xs


obstacleCycle :: IOGame' ()
obstacleCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do walls <- getObjectsFromGroup "walls"
                       --let walls' = take (n*2) walls
                       setObjectsAsleep walls True
                       setWallsPosition walls
    Level n ->  do walls <- getObjectsFromGroup "walls"
                   let walls' = take (n*4) walls
                   setObjectsAsleep walls' False
                   computeScore walls'
    GameOver -> do return()
    Win -> do walls <- getObjectsFromGroup "walls"
              setObjectsAsleep walls True