module Main where

import Graphics.UI.Fungen
import FlappyBirdClass
import ObjectSynonyms
import ObstacleClass
import ObstacleOperation
import GoldOperation
import Picture
import SizeSetting
import Text.Printf

gameCycle :: IOGame' ()
gameCycle = do
  obstacleCycle
  goldCycle
  flappyBirdCycle
  showScore
  gameState <- getGameState
  case gameState of
    LevelStart n -> do printOnScreen (printf "Level %d" n) TimesRoman24 middlePosition 1.0 1.0 1.0
    Level n -> do (GameAttribute score _ _ _ _) <- getGameAttribute
                  when (score >= 5*n) (do
                    if(n < 3) then do setGameState (LevelStart (n+1))
                    else do setGameState (Win)
                                     )
    GameOver -> do printOnScreen (printf "Defeat!") TimesRoman24 middlePosition 1.0 1.0 1.0
    Win -> do printOnScreen (printf "Victory!") TimesRoman24 middlePosition 1.0 1.0 1.0

stateControl :: Modifiers -> Position -> IOGame' ()
stateControl m p = do
  gameState <- getGameState
  case gameState of
    LevelStart n -> do setGameState (Level (n))
                       drawMap
    Level n -> do flying m p
    GameOver -> funExit
    Win -> funExit

showScore :: IOGame' ()
showScore = do
  (GameAttribute score _ goldNumber _ _) <- getGameAttribute
  gameState <- getGameState
  case gameState of
    LevelStart n -> return()
    Level n -> do printOnScreen (printf "Score: %d  Gold: %d  Level: %d" score goldNumber n) TimesRoman24 (40,(fromIntegral(snd windowSize)-60)) 1.0 1.0 1.0
    GameOver -> return()
    Win -> return()

main :: IO()
main =
  let winConfig = ((0,0), windowSize, "Flappy Bird")
      gameMap = textureMap 0 (fst backgroudSize) (snd backgroudSize) 500.0 500.0
      flappyBirds = objectGroup "flappyBird" [flappyBird]
      floors = objectGroup "floors" createFloorAndCeil
      walls = objectGroup "walls" createWalls
      golds = objectGroup "golds" createGolds
      input = [(SpecialKey KeyUp, Press, stateControl)]
      startingAttributes = GameAttribute 0 ((snd windowSize)`div`2) 0 False 0
  in funInit winConfig gameMap [flappyBirds, walls, floors, golds] (LevelStart 1) startingAttributes input gameCycle (Timer 40) bmList

