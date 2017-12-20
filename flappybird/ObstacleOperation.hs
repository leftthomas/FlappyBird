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

createFloorAndCeil :: [Floor]
createFloorAndCeil = [(object "floor" floor False floorPosition (0,0) ()),
                      (object "ceil" ceil False ceilPosition (0,0) ())]
                      where floor = Tex floorSize (getPictureIndex "floor" pictures)
                            ceil = Tex floorSize (getPictureIndex "ceil" pictures)

createWalls :: [Wall]
createWalls = [(object "topWall" wall False (wallRightPosition,y) (wallSpeed,0) ()),
               (object "downWall" wall False downWallPosition (wallSpeed,0) ())]
                      where wall = Tex wallSize (getPictureIndex "wall" pictures)
                            y = fromIntegral (unsafePerformIO $ randomRIO (390,850)::Int)
                            topWallPosition = (wallRightPosition,y)
                            downWallPosition = (wallRightPosition,wallDownPosition y)


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
    let y = fromIntegral (unsafePerformIO $ randomRIO (390,850)::Int)
    let y' = wallDownPosition y
    if(open == False) then do
      setObjectPosition (wallRightPosition, y) x
      setGameAttribute (GameAttribute (score+1) 1 goldNumber True lastCollisionGold)
    else do
      setObjectPosition (wallRightPosition, y') x
      setGameAttribute (GameAttribute score 1 goldNumber False lastCollisionGold)
                      )
  computeScore xs


obstacleCycle :: IOGame' ()
obstacleCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do walls <- getObjectsFromGroup "walls"
                       setObjectsAsleep walls True
                       setWallsPosition walls
    Level n ->  do walls <- getObjectsFromGroup "walls"
                   setObjectsAsleep walls False
                   computeScore walls
    GameOver -> do return()
    Win -> do walls <- getObjectsFromGroup "walls"
              setObjectsAsleep walls True