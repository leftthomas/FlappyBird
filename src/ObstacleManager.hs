module ObstacleManager(
  createWalls,
  obstacleManagerCycle, createFloor, stopWalls
) where

import Graphics.UI.Fungen
import Types
import Variables
import Textures
import Wall

wall1Speed = -10
wall1Hole = 125
minWallY = 100
maxWallY = fromIntegral((snd windowResolution) - 100)

createFloor :: [FBirdObject]
createFloor = let spriteFloor = Tex textureFloorSize textureFloorIndex
                  spriteCeil = Tex textureFloorSize textureCeilIndex
              in [(object "bottomFloor" spriteFloor False (floorBottomPosition) (0,0) ()),
                  (object "upperFloor" spriteCeil False floorUpperPosition (0,0) ())]

setObjectsAsleep :: [FBirdObject] -> Bool -> Int -> FBirdAction ()
setObjectsAsleep _ _ 0 = return ()
setObjectsAsleep [] _ _ = return ()
setObjectsAsleep (x:xs) k l = do
  setObjectAsleep k x
  setObjectsAsleep xs k (l-1)

checkOffScreen :: [FBirdObject] -> Int -> FBirdAction ()
checkOffScreen _  0 = return()
checkOffScreen [] _ = return()
checkOffScreen (x:xs) l = do
  (px, py) <- getObjectPosition x
  when (wallOffScreen px) (do
    (GA score lastWallY open) <- getGameAttribute
    if(open == False) then do
      let newY = ((lastWallY+(4*((snd windowResolution)`div`3)))`mod`(maxWallY-minWallY))+minWallY
      setObjectPosition (wallRightmostPosition, (wallFindUpPosition newY wall1Hole)) x
      setGameAttribute (GA (score+1) newY True)
    else do
      setObjectPosition (wallRightmostPosition, (wallFindDownPosition lastWallY wall1Hole)) x
      setGameAttribute (GA score lastWallY False)
                         )
  checkOffScreen xs (l-1)

stopWalls :: [FBirdObject] -> Int -> FBirdAction ()
stopWalls _ 0 = return()
stopWalls [] _ = return()
stopWalls (x:xs) l = do
  setObjectSpeed (0,0) x
  stopWalls xs (l-1)

retartWalls :: [FBirdObject] -> Int -> Int -> FBirdAction ()
retartWalls _ 0 _ = return()
restartWalls [] _ _ = return()
restartWalls (x:xs) l k = do
  (_, y) <- getObjectPosition x
  (GA score lastWallY open) <- getGameAttribute
  setObjectPosition (k, y) x
  if(open == False) then do
    setGameAttribute (GA score lastWallY True)
    restartWalls xs (l-1) k
  else do
    setGameAttribute (GA score lastWallY False)
    restartWalls xs (l-1) (k+475)

createWalls :: [FBirdObject]
createWalls = let sprite = Tex textureWallSize textureWallIndex
                  sprite2 = Tex textureWallSize textureWall2Index
              in [(object "wall11" sprite True (0, (wallFindUpPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed,0) ()),
                  (object "wall12" sprite True (0, (wallFindDownPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed, 0) ()),
                  (object "wall21" sprite2 True (0, (wallFindUpPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed,0) ()),
                  (object "wall22" sprite2 True (0, (wallFindDownPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed, 0) ()),
                  (object "wall31" sprite True (0, (wallFindUpPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed, 0) ()),
                  (object "wall32" sprite True (0, (wallFindDownPosition ((snd windowResolution)`div`2) wall1Hole)) (wall1Speed, 0) ())]

obstacleManagerCycle :: FBirdAction ()
obstacleManagerCycle = do
  gs <- getGameState
  case gs of
    LevelStart n -> do walls <- getObjectsFromGroup "walls"
                       setObjectsAsleep walls True (n*2)
                       restartWalls walls (n*2) wallRightmostPosition
    Level n ->  do walls <- getObjectsFromGroup "walls"
                   setObjectsAsleep walls False (n*2)
                   checkOffScreen walls (n*2)
    GameOver -> do return()
    Win -> do walls <- getObjectsFromGroup "walls"
              setObjectsAsleep walls True 4
