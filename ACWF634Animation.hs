module ACWF634Animation where

import Animation
import Data.List

--Screen properties
------------------------------------------------------------------------------------

width = 800
height = 600

--Math Functions
------------------------------------------------------------------------------------

pyDistance :: (Double,Double) -> (Animation, (Double, Double)) -> Double
pyDistance (x1,y1) (a,(x2,y2)) = sqrt (cx*cx + cy*cy)
 where
    cx = x1 - x2
    cy = y1 - y2
    
--Renders
------------------------------------------------------------------------------------    

--Black background
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Shape
cCircle :: Animation
cCircle = (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))

--Pattern: 20*15
pattern :: [(Animation, (Double,Double))]
pattern = [((translate (always (40*i,40*j)) cCircle), (i*40,j*40)) | i <- [0..20], j <- [0..15]]

--Sort Patterns closest to the center.
sortPattern :: [(Animation, (Double,Double))] -> [(Animation, (Double,Double))]
sortPattern xs = sortOn (pyDistance (400,300)) [(a,(x,y)) | (a,(x,y)) <- xs]

--Main picture
picture :: Animation
picture = background

--Helper/Debugger Functions
------------------------------------------------------------------------------------

coordPrinter :: [(Animation, (Double,Double))] -> [(Double,Double)]
coordPrinter xs = [(x,y) | (a,(x,y)) <- xs]


mark :: Animation
mark = (translate (always (400,300)) (withPaint (always red) (circle (always 20))))

--Outputs
------------------------------------------------------------------------------------

--Test animation individually
singleAnimation :: String -> Animation -> IO()
singleAnimation x y = writeFile x (svg width height y)

--RUN THIS TO PRODUCE THE ANIMATION!!!
outFile :: IO()
outFile = writeFile "tahmid_uddin_acwf634.svg" (svg width height picture)

--Compile using stack
main :: IO()
main = outFile