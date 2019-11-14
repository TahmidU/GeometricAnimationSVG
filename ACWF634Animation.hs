module ACWF634Animation where

import Animation
import Data.List

--Screen properties
------------------------------------------------------------------------------------

width = 800
height = 600

--Helper Functions
------------------------------------------------------------------------------------

--Helps to sort out the list containing: (Double,Double).
sortHelper :: (Double,Double) -> (Double, Double) -> Double
sortHelper (x1,y1) (x2,y2) = pyDistance (x1,y1) (x2,y2)

--Calculate distance from point to point.    
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
cCircle :: (Double,Double) -> Animation
cCircle (x,y) = (translate (always (x,y)) 
 (scale (repeatSmooth (1,1) [(1,(1,1)),(2,(2,2)),(3,(1,1))]) (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))))

--Pattern: 20*15
patternCoord :: [(Double,Double)]
patternCoord = [ (i*40,j*40) | i <- [0..20], j <- [0..15]]

--Sort Patterns closest to the center.
sortCoord :: [(Double,Double)] -> [(Double, Double)]
sortCoord xs = [ (x,y) |  (x,y) <- (sortOn (sortHelper (400,300)) xs)]

animatePattern :: [(Double,Double)] -> [Animation]
animatePattern xs = [cCircle (x,y) | (x,y) <- xs]

--Main picture
picture :: Animation
picture = background `plus` (combine (animatePattern (sortCoord patternCoord)))

--Experimental Picture
--pic :: Animation
--pic = background `plus` (scale (repeatSmooth (1,1) [(1,(1,1)),(2,(2,2)),(3,(1,1))]) (translate (always (360,300)) cCircle))
 --`plus` (scale (repeatSmooth (1,1) [(1,(1,1)),(2,(2,2)),(3,(1,1))]) (translate (always (400,300)) cCircle))

--Helper/Debugger Functions
------------------------------------------------------------------------------------

coordPrinter :: [(Double,Double)] -> [(Double,Double)]
coordPrinter xs = [(x,y) | (x,y) <- xs]

--distancePrinter :: [(Animation, Double)] -> [Double]
--distancePrinter xs = [d | (a,d) <- xs]

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