module ACWF634Animation where

import Animation

--Screen properties
width = 800
height = 600

--Black background
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Shape
cCircle :: Animation
cCircle = (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))

--Pattern: 20*15
pattern :: [Animation]
pattern = [(translate (always (40*i,40*j)) cCircle) | i <- [0..20], j <- [0..15]]

--Main picture
picture :: Animation
picture = background `plus` (combine pattern)

--Test animation individually
singleAnimation :: String -> Animation -> IO()
singleAnimation x y = writeFile x (svg width height y)

--RUN THIS TO PRODUCE THE ANIMATION!!!
outFile :: IO()
outFile = writeFile "tahmid_uddin_acwf634.svg" (svg width height picture)

main :: IO()
main = outFile