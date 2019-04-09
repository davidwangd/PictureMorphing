module Processor(
    getImage, findTriangle
)where

import Lib
import Codec.Picture
import Delaunay
import Control.Monad.Reader
import Debug.Trace
import Text.Printf

mixColor :: Double -> PixelRGBF -> PixelRGBF -> PixelRGBF
mixColor t (PixelRGBF r1 g1 b1) (PixelRGBF r2 g2 b2) = (PixelRGBF (f r1 r2) (f g1 g2) (f b1 b2))
    where f a b = a * (1.0-ft) + b * ft
          ft    = realToFrac t 
          
slice :: (Ord a) => a -> a -> a -> a 
slice low high value 
    | value < low = low
    | value > high = high
    | otherwise = value

-- 从Image 里面读取实数坐标的 像素值
innerPixel :: Image PixelRGBF -> Double -> Double -> PixelRGBF
innerPixel img fx fy = mixColor tx (mixColor ty lu ld) (mixColor ty ru rd)
    where px = slice 0 (imageWidth img - 2) (floor fx)
          py = slice 0 (imageHeight img - 2) (floor fy)
          tx = fx - (realToFrac px)
          ty = fy - (realToFrac py)
          -- 左上，右上，左下，右下
          -- 上下方向用 y 坐标， 左右方向用 x 坐标
          lu = pixelAt img px py
          ru = pixelAt img (px+1) py
          ld = pixelAt img px (py+1)
          rd = pixelAt img (px+1) (py+1)


getPos :: (Double,Double,Double) -> (Point,Point,Point) -> Point
getPos (u,v,w) ((x1,y1),(x2,y2),(x3,y3)) = (u*x1+v*x2+w*x3, u*y1+v*y2+w*y3) 

-- T时刻， 三角形重心坐标 三角形下标 
calcColor :: (Double,Double,Double) -> Triangle -> Reader MorphingInfo PixelRGBF
calcColor coord (p1, p2, p3) = do
    t    <- asks time
    lis1 <- asks srcPnt 
    lis2 <- asks tarPnt 
    src  <- asks srcImg
    tar  <- asks tarImg
    let (x1,y1) = getPos coord (lis1!!p1, lis1!!p2, lis1!!p3)
    let (x2,y2) = getPos coord (lis2!!p1, lis2!!p2, lis2!!p3) 
    return ${- trace (printf "Use Position (%.4f,%.4f)(%.4f,%.4f)" x1 y1 x2 y2) $-}
        mixColor t (innerPixel src x1 y1) (innerPixel tar x2 y2)

findTriangle :: Triangle -> Double -> Double -> Reader MorphingInfo Triangle
findTriangle hint x y = do
    points <- asks curPnt
    tris <- asks tris
    let lis = filter (\t->runReader (insideTriangle (x,y) t) points) tris 
    if runReader (insideTriangle (x,y) hint) points 
        then return hint 
    else if null lis 
        then return hint
        else return $ head lis 

calcPixel ::  Triangle -> Int -> Int -> Reader MorphingInfo (Triangle, PixelRGBF)
calcPixel hint x y = do
    t <- asks time
    let fx = realToFrac x
    let fy = realToFrac y 
    (a,b,c) <- findTriangle hint fx fy 
    points <- asks curPnt
    let coord = centerCoord (fx, fy) (points!!a, points!!b, points!!c)
    ret <- {-trace (printf "calc(%d,%d)" x y) $-} calcColor coord (a,b,c)
    return ((a,b,c), ret)

getImage :: MorphingInfo -> Image PixelRGBF
getImage info = snd $ generateFoldImage f (head $ tris info) w h 
    where f a b c = runReader (calcPixel a b c) info
          w       = imageWidth $ srcImg $ info
          h       = imageHeight $ srcImg $ info