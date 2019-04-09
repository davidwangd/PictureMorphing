module Lib
    ( Lib.readImage, readPoints,
      Context, MorphingInfo, readContext, buildMorphing,
      srcImg, srcPnt, tarImg, tarPnt, time, tris, curPnt
    ) where

import Codec.Picture
import Delaunay 
import Data.List.Split
import Text.Printf 

fromRGBAtoRGBF :: PixelRGBA8 -> PixelRGBF
fromRGBAtoRGBF (PixelRGBA8 r g b a) = PixelRGBF (f r) (f g) (f b)
    where f x = realToFrac(x) / 255.0

getImage :: (Either String DynamicImage) -> Image PixelRGBF
getImage (Right (ImageRGBA8 img)) = pixelMap fromRGBAtoRGBF img
getImage _ = generateImage fx 1 1
    where fx _ _ = PixelRGBF 0.0 0.0 0.0

readImage :: String -> IO (Image PixelRGBF)
readImage picName = Codec.Picture.readImage picName >>= return . getImage 

data Context = Context {
    img    :: Image PixelRGBF,
    points :: [Point]}
instance Show Context where
    show x = "Img RGBF : " ++ (show $ points x)

data MorphingInfo = MorphingInfo {
    srcImg :: Image PixelRGBF,
    srcPnt :: [Point],
    tarImg :: Image PixelRGBF,
    tarPnt :: [Point],
    tris   :: [Triangle],
    time   :: Double,
    curPnt :: [Point]}
instance Show MorphingInfo where
    show x = (concat $ map f2 (curPnt x)) ++ (concat $ map f3 (tris x))
        where f2 (x,y)   = printf "%.0f %.0f\n" x y
              f3 (x,y,z) = printf "%d %d %d\n" x y z

readPoints :: String -> IO [Point]
readPoints name = readFile name >>= return . map (\t -> (read t)::(Double,Double)) . init . splitOn "\n"

-- 获得图像边界的点。
boarderPoints :: (Pixel a) => Image a -> [Point]
boarderPoints img = [(0.0, 0.0), (w/2.0,0.0), (w,0.0), (0.0, h/2.0), (w, h/2.0), (0.0,h), (w/2.0,h), (w,h)]
    where h = realToFrac $ imageHeight img - 1
          w = realToFrac $ imageWidth img - 1

readContext :: String -> IO Context
readContext name = Lib.readImage (name ++ ".png") >>= 
    \img -> readPoints (name ++ ".txt") >>= 
        \points -> return $ Context{img=img, points=points ++ (boarderPoints img)}

-- 按照一个时间t转化两个坐标表
mixPosition :: Double -> [Point] -> [Point] -> [Point]
mixPosition t l1 l2 = map f (zip l1 l2)
    where f ((x1,y1),(x2,y2)) = (x1 * (1.0-t) + x2 * t, y1 * (1.0-t) + y2 * t)

-- 根据初始， 终止 读入一个Mophing结构体
buildMorphing :: Double -> Context -> Context -> MorphingInfo
buildMorphing t source target = MorphingInfo {
    srcImg = img source, 
    tarImg = img target,
    srcPnt = points source,
    tarPnt = points target,
    tris   = delaunay (points source),
    time   = t,
    curPnt = mixPosition t (points source) (points target)}