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
pictures = [(("backgroud",0::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/tex.bmp", black)),
            (("wall",1::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/wall.bmp", black)),
            (("ceil",2::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/floor.bmp", magenta)),
            (("floor",3::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/ceil.bmp", magenta)),
            (("flappyBird",4::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/player.bmp", black)),
            (("gold",5::Int),("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/gold.bmp", black))]

bmList = [("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/tex.bmp", black),
            ("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/wall.bmp", black),
            ("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/floor.bmp", magenta),
            ("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/ceil.bmp", magenta),
            ("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/player.bmp", black),
            ("/Users/lingxiaoxia/IdeaProjects/FlappyBird/assets/gold.bmp", black)]