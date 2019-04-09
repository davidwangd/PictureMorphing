module Delaunay(
    Point, Triangle,
    delaunay, insideCheck, area, testInsideCheck, centerCoord,
    insideTriangle
)where

import Control.Monad.Reader
import Data.List
import Debug.Trace

type Triangle = (Int,Int,Int)
type Point = (Double, Double)

eps :: Double
eps = 1e-4

superTriangle :: [Point]
superTriangle = [(0.0,-10000.0), (-10000.0, 10000.0), (10000.0, 10000.0)]

-- 计算三角形面积
area :: Point -> Point -> Point -> Double
area (x1,y1) (x2,y2) (x3,y3) = abs $ (x1-x2) * (y2-y3) - (y1-y2) * (x2-x3)

-- 计算三角形重心坐标
centerCoord :: Point -> (Point, Point, Point) -> (Double, Double, Double)
centerCoord p (a,b,c) = (area p b c / s, area p a c / s, area p a b / s)
    where s = area a b c

-- 根据三个点，得到外接圆的圆心
-- 采用 https://www.cnblogs.com/nobodyzhou/p/5521361.html 中的行列式计算方法
getCircle:: Point -> Point -> Point -> Point
getCircle (x0, y0) (x1, y1) (x2, y2) = (ax / (b*2.0), ay / (b*2.0))
    where ax = t0 * (y1 - y2) + t1 * (y2 - y0) + t2 * (y0 - y1)
          ay = x0 * (t1 - t2) + x1 * (t2 - t0) + x2 * (t0 - t1)
          b  = x0 * (y1 - y2) + x1 * (y2 - y0) + x2 * (y0 - y1)
          t0 = x0 * x0 + y0 * y0
          t1 = x1 * x1 + y1 * y1
          t2 = x2 * x2 + y2 * y2

-- 计算两个点的距离平方
dis2 :: Point -> Point -> Double
dis2 (x1, y1) (x2, y2) = (x1-x2) * (x1-x2) + (y1-y2) * (y1-y2)

-- 用于判断 给定的点，是否在给定的三角形外接圆内部。
insideCheck :: Point -> Triangle -> Reader [Point] Bool
insideCheck p (a,b,c) = do
    t <- ask
    let o = getCircle (t!!a) (t!!b) (t!!c)
    return $ dis2 p o <= dis2 o (t!!a)

insideTriangle :: Point -> Triangle -> Reader [Point] Bool
insideTriangle p (a1,b1,c1) = do
    t <- ask
    let (a,b,c) = (t!!a1,t!!b1,t!!c1)
    return $ abs ((area a b c) - ((area p a b) + (area p a c) + (area p b c))) < eps

-- 删除所有出现两次或者以上的元素
removeMultiple :: (Ord a) => [a] -> [a]
removeMultiple = removeRecursive . sort

removeRecursive :: (Ord a) => [a] -> [a]
removeRecursive [] = []
removeRecursive (x:xs) 
    | null xs      = [x]
    | head xs == x = removeRecursive $ dropWhile (==x) xs
    | otherwise    = x:(removeRecursive xs)

-- 对于所有的外接圆包含它的三角形，删除他们的公共边然后插入
insertPoint :: Int -> [Triangle] -> [Triangle]
insertPoint x lis = removeMultiple cur
    where cur = lis >>= f 
          f (a,b,c) = [(x,a,b), (x,b,c), (x,a,c)]
    
addPoint :: [Triangle] -> Int -> Reader [Point] [Triangle]
addPoint origin newPoint =  do
    dict <- ask
    let p = filter (\t -> runReader (insideCheck (dict!!newPoint) t) dict) origin
    let q = filter (\t -> not $ runReader (insideCheck (dict!!newPoint) t) dict) origin
    return $ (insertPoint newPoint p) ++ q

foldFunc :: [Point] -> [Triangle] -> Int -> [Triangle]
foldFunc dict origin newPoint = runReader (addPoint origin newPoint) dict

delaunay :: [Point] -> [Triangle]
delaunay points = filter f res
    where res = foldl (foldFunc $ points ++ superTriangle) [(l,l+1,l+2)] (reverse [0..l-1])
          l = length points 
          f (a,b,c) = and [(a<l),(b<l),(c<l)]
    
testInsideCheck :: String
testInsideCheck = "Test Circle\n" ++
    (show $ getCircle (0.0,0.0) (10.0,10.0) (0.0,20.0)) ++ "\n" ++ 
    (show $ getCircle (0.0,0.0) (1.0,1.0) (0.0,3.0))