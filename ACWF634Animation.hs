module ACWF634Animation where

import Animation

--Screen properties
width = 800
height = 600

centerW = 400
centerH = 300

--Black background
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Shape
hexagon :: Animation
hexagon = (withPaint (always white) (polygon [(0,50),(43.3,25),(43.3,-25),(0,-50),(-43.3,-25),(-43.3,25)]))

cCircle :: Animation
cCircle = (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))

--Main picture
picture :: Animation
picture = background

--Test animation individually
singleAnimation :: String -> Animation -> IO()
singleAnimation x y = writeFile x (svg width height y)

--RUN THIS TO PRODUCE THE ANIMATION!!!
outFile :: IO()
outFile = writeFile "tahmid_uddin_acwf634.svg" (svg width height picture)

main :: IO()
main = outFile