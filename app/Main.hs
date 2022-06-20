module Main where

import Data.Complex
import System.Random
import System.IO
import qualified Data.Array.IO as A
import Control.Monad


juliaMapLimit = 2.0 
xLimit = 2.0
yLimit = 2.0
nValuesPerLine = 1001
deltaX = 2.0 * xLimit / ( fromIntegral (nValuesPerLine - 1) )
deltaY = 2.0 * yLimit / ( fromIntegral (nValuesPerLine - 1) )

xValues = [ (negate xLimit) + fromIntegral i * deltaX | i <- [0 .. nValuesPerLine - 1] ]
yValues = [ (negate yLimit) + fromIntegral i * deltaY | i <- [0 .. nValuesPerLine - 1] ]


main = do
   putStrLn "Enter Re c:  "
   cRe <- fmap read getLine :: IO Double
   putStrLn "Enter Im c:  "
   cIm <- fmap read getLine :: IO Double
   putStrLn "Enter number of mapping iterations:  "
   n <- fmap read getLine :: IO Int
   let c = (cRe :+ cIm)
   h <- getOutFile
{-
   hPutStrLn h "x          y"
   mapM_ (zToGnuplotOut h) $ calcJulia c n
-}
   hPutStrLn h $ zToGnuplotOut' $ calcJulia c n
   hClose h 



getOutFile :: IO (Handle)
getOutFile = do
   putStrLn "Enter file name for output:  "
   fName <- getLine
   openFile fName WriteMode


calcJulia :: Complex Double -> Int -> [Complex Double]
calcJulia c n = foldr (\y juliaList -> juliaList ++ (getPointsAlongX y) ) [] yValues
   where getPointsAlongX yForLine = foldr (addPointIfJulia yForLine) [] xValues
         addPointIfJulia yValue xValue juliaListForLine = if isJulia c n (xValue :+ yValue) 
                                                             then (xValue :+ yValue) : juliaListForLine
                                                             else juliaListForLine  

isJulia :: Complex Double -> Int -> Complex Double -> Bool
isJulia c n z = isJulia' 0 z
   where isJulia' iterations zMapped
           |magnitude zMapped  > juliaMapLimit = False
           |iterations > n                     = True
           |otherwise                          = isJulia' (iterations + 1) (zMapped**2 + c)

zToGnuplotOut :: Handle -> Complex Double -> IO () 
zToGnuplotOut h z = hPutStrLn h (show (realPart z) ++ "   "   ++ show (imagPart z))

zToGnuplotOut' :: [Complex Double] -> String
zToGnuplotOut' zList = foldr (\(x :+ y) gnuOut -> (show x ++ "   " ++ show y ++ "\n") ++ gnuOut)  ("x          y\n") zList
 
 


mapN :: Complex Double -> Int -> Complex Double -> Complex Double
mapN c n z = foldr (\ _ mappedZ -> mappedZ**2 + c) z [1 .. n]
