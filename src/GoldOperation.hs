module GoldOperation(
    goldSpeed,
    createGolds,
    setGoldsAsleep,
    stopMovingGolds,
    updateGoldsPosition,
    goldCycle,
    computeGolds
) where

import Graphics.UI.Fungen
import ObjectSynonyms
import Picture
import SizeSetting
import ObstacleClass
import System.IO.Unsafe
import System.Random

goldSpeed = -10

createGolds :: [Gold]
createGolds = [(object "gold1" gold False (unsafePerformIO $ randomRIO (100.0,200.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold2" gold False (unsafePerformIO $ randomRIO (210.0,310.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold3" gold False (unsafePerformIO $ randomRIO (320.0,420.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold4" gold False (unsafePerformIO $ randomRIO (301.0,400.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold5" gold False (unsafePerformIO $ randomRIO (401.0,600.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold6" gold False (unsafePerformIO $ randomRIO (501.0,600.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold7" gold False (unsafePerformIO $ randomRIO (601.0,700.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold8" gold False (unsafePerformIO $ randomRIO (701.0,800.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ()),
               (object "gold9" gold False (unsafePerformIO $ randomRIO (801.0,900.0),unsafePerformIO $ randomRIO (50.0,600.0)) (goldSpeed,0) ())]
                      where gold = Tex goldSize (getPictureIndex "gold" pictures)


setGoldsAsleep :: [Gold] -> Bool -> IOGame' ()
setGoldsAsleep [] _  = return ()
setGoldsAsleep (x:xs) asleep = do
  setObjectAsleep asleep x
  setGoldsAsleep xs asleep


stopMovingGolds :: [Gold]  -> IOGame' ()
stopMovingGolds [] = return()
stopMovingGolds (x:xs) = do
  setObjectSpeed (0,0) x
  stopMovingGolds xs

updateGoldsPosition :: [Gold]  -> IOGame' ()
updateGoldsPosition [] = return()
updateGoldsPosition (x:xs) = do
  (px, py) <- getObjectPosition x
  when (isGoldOut px) (do
    (GameAttribute score lastWallY goldNumber open lastCollisionGold) <- getGameAttribute
    setObjectPosition (wallRightPosition,500) x
    setGameAttribute (GameAttribute score lastWallY goldNumber open lastCollisionGold)
                      )
  updateGoldsPosition xs


goldCycle :: IOGame' ()
goldCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do golds <- getObjectsFromGroup "golds"
                       setGoldsAsleep golds True
    Level n ->  do golds <- getObjectsFromGroup "golds"
                   setGoldsAsleep golds False
                   updateGoldsPosition golds
                   computeGolds golds 1
    GameOver -> do return()
    Win -> do golds <- getObjectsFromGroup "golds"
              setGoldsAsleep golds True

isGoldOut :: Double -> Bool
isGoldOut position
    | position > wallLeftPosition = False
    | otherwise = True


computeGolds :: [Gold] -> Int -> IOGame' ()
computeGolds [] _ = return ()
computeGolds _ 10 = return ()
computeGolds (x:xs) y = do
        flappyBirds <- getObjectsFromGroup "flappyBird"
        isCollision <- objectListObjectCollision flappyBirds x
        (GameAttribute score lastWallY goldNumber open lastCollisionGold) <- getGameAttribute
        when (isCollision) (do
            if (lastCollisionGold /= y) then
                 setGameAttribute (GameAttribute score lastWallY (goldNumber + 1) open y)
            else
                 setGameAttribute (GameAttribute score lastWallY goldNumber open lastCollisionGold)
                           )
        computeGolds xs (y+1)
