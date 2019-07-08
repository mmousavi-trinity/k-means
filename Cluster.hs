module Cluster where
import ClusterData (tenPoints)
import Data.List
import Data.Maybe
import Debug.Trace

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test 
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--

--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.
getKElems :: Int -> [a] -> [a]
getKElems k [] = error"Empty list" 
getKElems k lst = take k lst
              
--Example: getKElems 3 [1..6] = [1,3,6]

--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
eucDist :: Point -> Point -> Double
eucDist (a1,a2,a3) (b1,b2,b3) = sqrt (x*x + y*y)
                where x = (b1-a1)  
                      y = (b2-a2)
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist (a1,a2,a3) (b1,b2,b3) = abs(a1-b1) + abs(a2-b2)
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist (a1,a2,a3) (b1,b2,b3) = maximum[abs(a1-b1), abs(a2-b2)]
--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist (a1,a2,a3) (b1,b2,b3) = (horD*2) + vertD
                where horD = abs(a1-b1)
                      vertD = abs(a2-b2)
--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist (a1,a2,a3) (b1,b2,b3) = if a3 == b3 then abs(manhatDist (a1,a2,a3) (b1,b2,b3)) 
                                     else abs(2*manhatDist (a1,a2,a3) (b1,b2,b3))  
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
summ :: [Double] -> Double
summ (x:xs) = x + summ xs
average :: [Double] -> Double
average (x:xs) = sum (x:xs)/fromIntegral (length (x:xs))
--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: Ord a => (a -> Double) -> [a] -> a
minimize f [] = error"Error: empty list "
minimize f [x] = x 
minimize f (x:xs)  = let minim = minimize f xs 
                     in if (f x) < (f minim) then x else minim 
--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucket function, a list of items to be placed in buckets, and a list of buckets, return
--the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF.
--
--
--buildCorpusHelper :: Digit ->[(PixelImage, Digit)] -> [PixelImage]
--buildCorpusHelper digit imgdata = [ a|(a,b) <- imgdata, b == digit] 
--buildCorpus :: [(PixelImage, Digit)] -> Corpus
--buildCorpus imgLbls = [(digit, buildCorpusHelper digit imgLbls)|digit <-[0..9], length (buildCorpusHelper digit imgLbls) > 0]
--bucketHelper :: Eq b => [(b,[a])] -> [(b,[a])]
--bucketHelper (x:xs) = if fst x == fst (head xs) 
  --                    then do
    --                  (b,[snd x:snd (head xs)])
      --                 bucketHelper xs  
        --              else
          --             bucketHelper xs 
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket f bs items = [(b,[item|item<-items,f item == b])|b<-bs]  
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint f centers point = minimize (f point) centers
--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints f centers points  =  bucket pointFunct centers points
                                 where pointFunct point = assignPoint f centers point
--assignPointHelper :: Eq a => a -> a 

--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


--Given a metric and a  cluster, return the mean of the points in the cluster.
--The label should be the label of the closest point to the new center.
--Do NOT CONSIDER the current center to be a point. It does not count.
--Since you can't take the mean of an empty list of points, return Nothing in that case.
--

findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean f (center,[]) = Nothing
findMean f (center,points) = Just (p1,p2,p3)
                          where
                             p1 = average [x|(x,y,label)<-points]
                             p2 = average [y|(x,y,label)<-points]
                             p3 = getThird (assignPoint f points (p1,p2,-1))
                             getThird (_,_,l) = l                             
--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, return all the valid centers. If any cluster is empty,
--simply remove .
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
 
moveCenters f [] = [] 
moveCenters f (cluster:clusters) = case findMean f cluster of
                                         Nothing->moveCenters f clusters
                                         Just c -> c:moveCenters f clusters 
--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website
--
--clusters - return n centers. if the length of centers is = to k 
--
improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters f k (cluster:clusters) = assignPoints f cs ps 
                                         where ps =  [point|(center,points)<-(cluster:clusters),point<-points]
                                               cs = moveCenters f (cluster:clusters)                                          
--Example: let newClusters = improveClusters trafficDist 2 testClusters 
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
--improve them, until they stop changing.
--
kMeansHelper ::(Point -> Center->Double) -> Int->[Cluster]->[Cluster] 
kMeansHelper f k clusters = if k /= iterationLimit 
                          then kMeansHelper f (k+1) (improveClusters f k clusters)
                          else clusters
kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans f k points = kMeansHelper f k (assignPoints f (getKElems k points) points)
