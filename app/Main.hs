module Main where

import Lib
import Processor
import Codec.Picture
import Text.Printf 

printFunc :: Context -> Context -> Int -> IO ()
printFunc source target t = putStrLn (printf "solving %d..." t) >>
    savePngImage (printf "sources/result1/%d.png" t) (ImageRGBF (getImage $ buildMorphing (realToFrac t / 64.0) source target))

main = do 
    putStrLn "Compiled Success!, 请输入位置信息名称"
    source <- readContext "sources/source1"
    target <- readContext "sources/target1"
    mapM_ (printFunc source target) [0..64]