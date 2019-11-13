module ACWF634Animation where

import Animation
import Data.List

--Screen properties
------------------------------------------------------------------------------------

width = 800
height = 600

--Math Functions
------------------------------------------------------------------------------------
--Helper for sorting animations by calculating distance from point to center.
sortDistanceHelper :: (Double,Double) -> (Animation, (Double, Double)) -> Double
sortDistanceHelper (x1,y1) (a,(x2,y2)) = sqrt (cx*cx + cy*cy)
 where
    cx = x1 - x2
    cy = y1 - y2

--Calculate distance from point to center.    
pyDistance :: (Double,Double) -> (Double, Double) -> Double
pyDistance (x1,y1) (x2,y2) = sqrt (cx*cx + cy*cy)
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
sortPattern :: [(Animation, (Double,Double))] -> [(Animation, Double)]
sortPattern xs = [ (a,(pyDistance (400,300) (x,y))) |  (a,(x,y)) <- (sortOn (sortDistanceHelper (400,300)) xs)]

--Main picture
picture :: Animation
picture = background

--Experimental Picture
pic :: Animation
pic = scale (cycleSmooth 2 [(0,0),(1,1)]) (withPaint (always green) (circle (always 20)))

--Helper/Debugger Functions
------------------------------------------------------------------------------------

coordPrinter :: [(Animation, (Double,Double))] -> [(Double,Double)]
coordPrinter xs = [(x,y) | (a,(x,y)) <- xs]

distancePrinter :: [(Animation, Double)] -> [Double]
distancePrinter xs = [d | (a,d) <- xs]

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