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

--Renders
------------------------------------------------------------------------------------    

--Black background
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Shape
cCircle :: (Double,Double) -> Int -> Animation
cCircle (x,y) i = (translate (always (x,y)) 
 (scale (repeatSmooth (1,1) [(fromIntegral (i::Int) + 1,(1,1)),(fromIntegral (i::Int) + 2,(2,2)),( fromIntegral (i::Int) + 3,(1,1))]) (withBorder (always white) (always 2) (withoutPaint (circle (always 20))))))

--Pattern: 20*15
produceCoords :: [(Double,Double)]
produceCoords = [ (i*40,j*40) | i <- [0..20], j <- [0..15]]

--Sort Patterns closest to the center.
sortCoords :: [(Double,Double)] -> [(Double, Double)]
sortCoords xs = [ (x,y) |  (x,y) <- (sortOn (sortHelper (400,300)) xs)]

sortDistance :: [(Double,Double)] -> [Double]
sortDistance xs = [pyDistance (400,300) (x,y) | (x,y) <- (sortOn(sortHelper (400,300)) xs)]

groupCoords :: [Int]
groupCoords = fmap length (group (sortDistance produceCoords))

animatePattern :: [(Double,Double)] -> [Animation]
animatePattern xs = [(cCircle (x,y) i) | (x,y) <- xs, i <- [0..336]]

--Main picture
picture :: Animation
picture = background `plus` (combine (animatePattern (sortCoords produceCoords)))

--Experimental Picture
pic :: Animation
pic = background 

--Helper/Debugger Functions
------------------------------------------------------------------------------------

coordPrinter :: [(Double,Double)] -> [(Double,Double)]
coordPrinter xs = [(x,y) | (x,y) <- xs]

--distancePrinter :: [(Animation, Double)] -> [Double]
--distancePrinter xs = [d | (a,d) <- xs]

mark :: Animation
mark = (translate (always (400,300)) (withPaint (always red) (circle (always 20))))

dummy :: [(Double,Double)] -> [Int] -> [Int]
dummy xs ys = [y | x <- xs, y <- ys]

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