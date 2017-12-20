module FlappyBirdClass(
    createObjectPicture,
    createFlappyBird,
    addGravity,
    flappyBirdCycle,
    judgeCollisions,
    flying,
    flappyBird,
    gravity,
    speed,
    fall
) where

import Graphics.UI.Fungen
import ObjectSynonyms
import ObstacleOperation
import Picture
import SizeSetting
import GoldOperation

gravity = 1.0
fall = -10.0
speed = (0, 10.0)
startPosition = (fromIntegral((fst windowSize)`div`8), fromIntegral((snd windowSize)`div`2))
flappyBirdPicture :: ObjectPicture
flappyBirdPicture = createObjectPicture flappyBirdSize (getPictureIndex "flappyBird" pictures)
flappyBird = createFlappyBird "flappyBird" flappyBirdPicture False startPosition (0,0)

--create flappy birds' object picture using ObjectPicture in Fungen
createObjectPicture :: (Double,Double) -> Int -> ObjectPicture
createObjectPicture = \(x,y) index -> Tex (x,y) index

--create flappy bird object using object function in Fungen Objects.hs
createFlappyBird :: String -> ObjectPicture -> Bool -> (Double,Double) -> (Double,Double) -> FlappyBird
createFlappyBird = \name picture asleep position initSpeed -> object name picture asleep position initSpeed ()

--flying: change the position of flappy bird
flying :: Modifiers -> Position -> IOGame' ()
flying _ _  = do
  flappyBird <- findObject "flappyBird" "flappyBird"
  setObjectSpeed speed flappyBird

--add gravity to flappy bird
addGravity :: IOGame' ()
addGravity = do
  flappyBird <- findObject "flappyBird" "flappyBird"
  (x, y) <- getObjectSpeed flappyBird
  when (y > fall) (do setObjectSpeed (x, (y-gravity)) flappyBird)


flappyBirdCycle :: IOGame' ()
flappyBirdCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do flappyBird <- findObject "flappyBird" "flappyBird"
                       setObjectAsleep True flappyBird
    Level n -> do flappyBird <- findObject "flappyBird" "flappyBird"
                  setObjectAsleep False flappyBird
                  addGravity
                  judgeCollisions
    GameOver -> do return()
    Win -> do flappyBird <- findObject "flappyBird" "flappyBird"
              setObjectAsleep True flappyBird

judgeCollisions :: IOGame' ()
judgeCollisions = do
  flappyBird <- findObject "flappyBird" "flappyBird"
  floor <- getObjectsFromGroup "floors"
  floorCol <- objectListObjectCollision floor flappyBird
  walls <- getObjectsFromGroup "walls"
  wallCol <- objectListObjectCollision walls flappyBird
  golds <- getObjectsFromGroup "golds"
  goldCol <- objectListObjectCollision golds flappyBird
  when (floorCol || wallCol) (do
    setObjectSpeed (0,0) flappyBird
    stopMovingWalls walls
    stopMovingGolds golds
    setGameState (GameOver)
                            )




