module Coursework where

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

--Set the animations times.
animationTimes :: Int -> [(Double, (Double,Double))]
animationTimes i = [if (n < 30 || n > 95) then ((fromIntegral (i::Int) + n)*speed,(1,1)) else ((fromIntegral (i::Int) + n)*speed,(2,2)) | n <- [1..100]]  
 where
    speed = 0.02

--Renders
------------------------------------------------------------------------------------    

--Black background.
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Custom Circle, allows me to easily recreate it.
customCircle :: [(Double,Double)] -> Int -> [Animation]
customCircle xs i = [(translate (always (x,y))
 (scale (repeatSmooth (1,1) (animationTimes i))
 (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))))
 | (x,y) <- xs]

--Pattern: 20*15
produceCoords :: [(Double,Double)]
produceCoords = [ (i*40,j*40) | i <- [0..20], j <- [0..15]]

--Sort Patterns closest to the center.
sortCoords :: [(Double,Double)] -> [(Double, Double)]
sortCoords xs = [ (x,y) |  (x,y) <- (sortOn (sortHelper (400,300)) xs)]

sortDistance :: [(Double,Double)] -> [Double]
sortDistance xs = [pyDistance (400,300) (x,y) | (x,y) <- (sortOn(sortHelper (400,300)) xs)]

--Zip coords that are equal distance away from the center and then index that list.
groupCoords :: [(Int,[(Double,Double)])]
groupCoords = zip [0..] (groupBy (\(x1,y1) (x2,y2) -> (pyDistance (400,300) (x1,y1)) == (pyDistance (400,300) (x2,y2))) (sortCoords produceCoords))

--Apply animation.
animate :: [(Int,[(Double,Double)])] -> [Animation]
animate xs = concat [customCircle cs i | (i,cs) <- xs] 

--Main picture.
picture :: Animation
picture = background `plus` (combine (animate groupCoords)) 

--Outputs
------------------------------------------------------------------------------------

--Test animation individually.
singleAnimation :: String -> Animation -> IO()
singleAnimation x y = writeFile x (svg width height y)

--RUN THIS TO PRODUCE THE ANIMATION!!!
outFile :: IO()
outFile = writeFile "tahmid_uddin_acwf634.svg" (svg width height picture)

--Compile using stack.
main :: IO()
main = outFile