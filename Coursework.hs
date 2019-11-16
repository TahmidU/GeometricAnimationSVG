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

trd :: (a,b,c) -> c
trd (_,_,z) = z    

--Renders
------------------------------------------------------------------------------------    

--Black background
background :: Animation
background = translate (always (0,0))
 (withPaint (always black) (rect (always 800) (always 600)))

--Shape
customCircle :: [(Double,Double)] -> Int -> [Animation]
customCircle xs i = [(translate (always (x,y))
 (scale (repeatSmooth (1,1) [((fromIntegral (i::Int) + 1)*0.06,(1,1)),((fromIntegral (i::Int) + 2)*0.06,(2,2)),((fromIntegral (i::Int) + 3)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 4)*0.06,(2,2)), ((fromIntegral (i::Int) + 5)*0.06,(2,2)), ((fromIntegral (i::Int) + 6)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 7)*0.06,(2,2)), ((fromIntegral (i::Int) + 8)*0.06,(2,2)), ((fromIntegral (i::Int) + 9)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 10)*0.06,(2,2)), ((fromIntegral (i::Int) + 11)*0.06,(2,2)), ((fromIntegral (i::Int) + 12)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 13)*0.06,(2,2)), ((fromIntegral (i::Int) + 14)*0.06,(2,2)), ((fromIntegral (i::Int) + 15)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 16)*0.06,(2,2)), ((fromIntegral (i::Int) + 17)*0.06,(2,2)), ((fromIntegral (i::Int) + 18)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 19)*0.06,(2,2)), ((fromIntegral (i::Int) + 20)*0.06,(2,2)), ((fromIntegral (i::Int) + 21)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 22)*0.06,(2,2)), ((fromIntegral (i::Int) + 23)*0.06,(2,2)), ((fromIntegral (i::Int) + 24)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 25)*0.06,(2,2)), ((fromIntegral (i::Int) + 26)*0.06,(2,2)), ((fromIntegral (i::Int) + 27)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 28)*0.06,(2,2)), ((fromIntegral (i::Int) + 29)*0.06,(2,2)), ((fromIntegral (i::Int) + 30)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 31)*0.06,(2,2)), ((fromIntegral (i::Int) + 32)*0.06,(2,2)), ((fromIntegral (i::Int) + 33)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 34)*0.06,(2,2)), ((fromIntegral (i::Int) + 35)*0.06,(2,2)), ((fromIntegral (i::Int) + 36)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 37)*0.06,(2,2)), ((fromIntegral (i::Int) + 38)*0.06,(2,2)), ((fromIntegral (i::Int) + 39)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 40)*0.06,(2,2)), ((fromIntegral (i::Int) + 41)*0.06,(2,2)), ((fromIntegral (i::Int) + 42)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 43)*0.06,(2,2)), ((fromIntegral (i::Int) + 44)*0.06,(2,2)), ((fromIntegral (i::Int) + 45)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 46)*0.06,(2,2)), ((fromIntegral (i::Int) + 47)*0.06,(2,2)), ((fromIntegral (i::Int) + 48)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 49)*0.06,(2,2)), ((fromIntegral (i::Int) + 50)*0.06,(2,2)), ((fromIntegral (i::Int) + 51)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 52)*0.06,(2,2)), ((fromIntegral (i::Int) + 53)*0.06,(2,2)), ((fromIntegral (i::Int) + 54)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 55)*0.06,(2,2)), ((fromIntegral (i::Int) + 56)*0.06,(2,2)), ((fromIntegral (i::Int) + 57)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 58)*0.06,(2,2)), ((fromIntegral (i::Int) + 59)*0.06,(2,2)), ((fromIntegral (i::Int) + 60)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 61)*0.06,(2,2)), ((fromIntegral (i::Int) + 62)*0.06,(2,2)), ((fromIntegral (i::Int) + 63)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 64)*0.06,(2,2)), ((fromIntegral (i::Int) + 65)*0.06,(2,2)), ((fromIntegral (i::Int) + 66)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 67)*0.06,(2,2)), ((fromIntegral (i::Int) + 68)*0.06,(2,2)), ((fromIntegral (i::Int) + 69)*0.06,(2,2)),
 ((fromIntegral (i::Int) + 70)*0.06,(2,2)), ((fromIntegral (i::Int) + 71)*0.06,(1,1)), ((fromIntegral (i::Int) + 72)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 73)*0.06,(1,1)), ((fromIntegral (i::Int) + 74)*0.06,(1,1)), ((fromIntegral (i::Int) + 75)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 76)*0.06,(1,1)), ((fromIntegral (i::Int) + 77)*0.06,(1,1)), ((fromIntegral (i::Int) + 78)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 79)*0.06,(1,1)), ((fromIntegral (i::Int) + 80)*0.06,(1,1)), ((fromIntegral (i::Int) + 81)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 82)*0.06,(1,1)), ((fromIntegral (i::Int) + 83)*0.06,(1,1)), ((fromIntegral (i::Int) + 84)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 85)*0.06,(1,1)), ((fromIntegral (i::Int) + 86)*0.06,(1,1)), ((fromIntegral (i::Int) + 87)*0.06,(1,1)),
 ((fromIntegral (i::Int) + 88)*0.06,(1,1)), ((fromIntegral (i::Int) + 89)*0.06,(1,1)), ((fromIntegral (i::Int) + 90)*0.06,(1,1))])
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

-- zip: (Index, equal distances, (x,y))
groupCoords :: [(Int,[(Double,Double)])]
groupCoords = zip [0..] (groupBy (\(x1,y1) (x2,y2) -> (pyDistance (400,300) (x1,y1)) == (pyDistance (400,300) (x2,y2))) (sortCoords produceCoords))

animate :: [(Int,[(Double,Double)])] -> [Animation]
animate xs = concat [customCircle cs i | (i,cs) <- xs] 

--Main picture
picture :: Animation
picture = background `plus` (combine (animate groupCoords)) 

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