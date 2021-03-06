module Picture(
    color,
    getPictureIndex,
    black,
    magenta,
    pictures,
    bmList
) where

import Graphics.UI.Fungen

--set the color use InvList in Fungen
color :: Int -> Int -> Int -> InvList
color r g b = Just [(r, g, b)]

-- get the index in pictures for use of Tex
getPictureIndex :: String -> [((String,Int),(String,InvList))] -> Int
getPictureIndex pictureName (x:xs)
  | null xs = index
  | pictureName == pictureName' = index
  | otherwise = getPictureIndex pictureName xs
  where
    tuple = fst x
    pictureName' = fst tuple
    index = snd tuple


--init the color
black = color 0 0 0
magenta = color 255 0 255
pictures = [(("backgroud",0::Int),("pictures/background.bmp", black)),
            (("wall",1::Int),("pictures/wall.bmp", black)),
            (("ceil",2::Int),("pictures/ceil.bmp", magenta)),
            (("floor",3::Int),("pictures/floor.bmp", magenta)),
            (("flappyBird",4::Int),("pictures/bird.bmp", black)),
            (("gold",5::Int),("pictures/gold.bmp", black))]

bmList = [("pictures/background.bmp", black),
            ("pictures/wall.bmp", black),
            ("pictures/floor.bmp", magenta),
            ("pictures/ceil.bmp", magenta),
            ("pictures/bird.bmp", black),
            ("pictures/gold.bmp", black)]