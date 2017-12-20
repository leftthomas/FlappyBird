module Picture(
    color,
    getPictureIndex,
    black,
    magenta,
    pictures,
    bmList
) where

import Graphics.UI.Fungen

--set the color use InvList in Fungen Types.hs
color :: Int -> Int -> Int -> InvList
color = \r g b -> Just [(r,g,b)]

-- get the index in pictures for use of Tex
getPictureIndex :: String -> [((String,Int),(String,InvList))] -> Int
getPictureIndex pictureName (x:xs) = if length xs == 0 then index
                                        else if  pictureName == pictureName' then index
                                            else getPictureIndex pictureName xs
                                                where tuple = fst x
                                                      pictureName' = fst tuple
                                                      index = snd tuple


--init the color
black = color 0 0 0
magenta = color 255 0 255
pictures = [(("backgroud",0::Int),("assets/tex.bmp", black)),
            (("wall",1::Int),("assets/wall.bmp", black)),
            (("ceil",2::Int),("assets/floor.bmp", magenta)),
            (("floor",3::Int),("assets/ceil.bmp", magenta)),
            (("flappyBird",4::Int),("assets/player.bmp", black)),
            (("gold",5::Int),("assets/gold.bmp", black))]

bmList = [("assets/tex.bmp", black),
            ("assets/wall.bmp", black),
            ("assets/floor.bmp", magenta),
            ("assets/ceil.bmp", magenta),
            ("assets/player.bmp", black),
            ("assets/gold.bmp", black)]