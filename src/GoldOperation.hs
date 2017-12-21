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
createGolds = [(object "gold1" gold False (unsafePerformIO $ randomRIO (150.0,200.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold2" gold False (unsafePerformIO $ randomRIO (250.0,300.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold3" gold False (unsafePerformIO $ randomRIO (350.0,400.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold4" gold False (unsafePerformIO $ randomRIO (450.0,500.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold5" gold False (unsafePerformIO $ randomRIO (550.0,600.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold6" gold False (unsafePerformIO $ randomRIO (650.0,700.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold7" gold False (unsafePerformIO $ randomRIO (750.0,800.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ()),
               (object "gold8" gold False (unsafePerformIO $ randomRIO (850.0,900.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ())]
            --   (object "gold9" gold False (unsafePerformIO $ randomRIO (950.0,1000.0),unsafePerformIO $ randomRIO (100.0,400.0)) (goldSpeed,0) ())]
                      where gold = Tex goldSize (getPictureIndex "gold" pictures)

setGoldsPosition :: [Gold] -> Int -> IOGame' ()
setGoldsPosition [] _ = return()
setGoldsPosition _ 0 = return()
setGoldsPosition (x:xs) n = do
    (px, py) <- getObjectPosition x
    (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
    let y = (take 1 $ randomRs (200,500) (mkStdGen tempY) :: [Int])!!0
    setObjectPosition (px, fromIntegral(y)) x
    setGameAttribute (GameAttribute score wallSpace goldNumber down y lastCollisionGold)
    setGoldsPosition xs (n - 1)


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
  if (isGoldOut px) then (do
    (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
    let newY = (take 1 $ randomRs (200,500) (mkStdGen tempY) :: [Int])!!0
    setObjectPosition (wallRightPosition + px, fromIntegral(newY)) x
    setGameAttribute (GameAttribute score wallSpace goldNumber down newY lastCollisionGold)
                      )
  else do
    (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
    setGameAttribute (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold)
  updateGoldsPosition xs


goldCycle :: IOGame' ()
goldCycle = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do golds <- getObjectsFromGroup "golds"
                       setGoldsAsleep golds True
                       updateGoldsPosition golds
                       setGoldsPosition golds 9
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
computeGolds _ 9 = return ()
computeGolds (x:xs) y = do
        (px, py) <- getObjectPosition x
        flappyBirds <- getObjectsFromGroup "flappyBird"
        isCollision <- objectListObjectCollision flappyBirds x
        (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold) <- getGameAttribute
        if (isCollision) then (do
            let newY = ((take 1 $ randomRs (200,500) (mkStdGen tempY) :: [Int])!!0)
            setObjectPosition (wallRightPosition + px, fromIntegral(newY)) x
            if (lastCollisionGold /= y) then (do
                 setGameAttribute (GameAttribute score wallSpace (goldNumber+1) down newY y)
                                           )
            else do
                 setGameAttribute (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold)
                           )
        else do
             setGameAttribute (GameAttribute score wallSpace goldNumber down tempY lastCollisionGold)
        computeGolds xs (y+1)

