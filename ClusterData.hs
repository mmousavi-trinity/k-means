module ClusterData where
import Data.List.Split
import Data.List
import Data.Tuple
import Debug.Trace

tenPoints :: [(Double,Double,Int)]
tenPoints = [(7,1,100), (7,3,200), (8,1,300), (8,2,400), (7.5,3.5,500), (2,7,600), (1,7,700), (3,7,800), (2,8,900), (2,6,1000)]

--readPoint :: String -> [(String, Int)] -> (Point, [(String, Int)])
readPoint line labels =
    let (x:y:rest) = splitOn "," line
        (labelVal, label) = makeLabel rest labels
        point = (read x, read y, label)
        newLabels = nub ((labelVal, label):labels)
    in (point, newLabels)

freshLabel labels = 1+(maximum $ 0:(map snd labels))

makeLabel rest labels = 
    let x = case length rest of
                0 -> show (freshLabel labels)
                1 -> head rest
                5 -> rest!!3
                k -> unwords rest
    in case lookup x labels of
        Nothing -> (x, freshLabel labels)
        Just y -> (x,y)

--readPoints :: [String] -> ([Point],[(Int, String)])
readPoints strs = (points, map swap assocs)
    where readFold (pnts,lbls) s = let (p,lbls') = readPoint s lbls in (p:pnts, lbls')
          (points, assocs) =  foldl readFold ([],[]) strs

pointsOfFile n fname = do
    contents <- readFile fname
    return $ readPoints $ take n $ lines contents

range lst = (minimum lst, maximum lst)

--squarePoints :: [Point] -> [Point]
squarePoints points =
    let (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        xdif = xmax - xmin
        ydif = ymax - ymin
        xScale xval = (10*(xval-xmin))/xdif
        yScale yval = (10*(yval-ymin))/ydif
    in map (\(x,y,l) -> (xScale x, yScale y, l)) points

--scalePoints :: [Point] -> [Point]
scalePoints points =
    let (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        factor = min (10/(xmax-xmin)) (10/(ymax-ymin))
        xScale xval = factor * (xval-xmin)
        yScale yval = factor * (yval-ymin)
    in map (\(x,y,l) -> (xScale x, yScale y, l)) points

--scaleClusters :: [Cluster] -> [Cluster]
scaleClusters clusters =
    let points = concatMap snd clusters
        (xmin, xmax) = range $ map (\(x,_,_) -> x) points
        (ymin, ymax) = range $ map (\(_,y,_) -> y) points
        factor = min (10/(xmax-xmin)) (10/(ymax-ymin))
        xScale xval = factor * (xval-xmin)
        yScale yval = factor * (yval-ymin)
        scalePoint (x,y,l) = (xScale x, yScale y, l)
        scaleCluster (center,cpoints) = (scalePoint center, map scalePoint cpoints) 
    in map scaleCluster clusters 
