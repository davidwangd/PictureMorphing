import Delaunay
import Lib
import Data.List.Split 
import Processor
import Control.Monad.Reader 

list :: [Point]
list = fmap (fmap realToFrac) [(5,0), (5,5),(3,2),(4,2),(1,1), (3,3),(0,3),(1,4),(1,5)]

filename = "sources/source1.txt"

main :: IO ()
main = do 
    putStrLn "Compiled Success!, 请输入位置信息名称"
    source <- readContext "sources/source1"
    target <- readContext "sources/target1"
    let info = buildMorphing 0.5 source target
    putStrLn $ show $ runReader (findTriangle (0,0,1) 300.0 400.0) info
