module Testing where
--todo: truncate numbers.
import Data.Time
import Debug.Trace
import Control.DeepSeq
import Data.Maybe
import System.CPUTime
import Control.Monad
import Control.Exception
import Data.List
import Coloring
import Data.Either
import Cluster
import ClusterData

type TestCase = IO (Either String String)

assertTrue :: Bool -> TestCase
assertTrue b =  do
    ret <- (try $ evaluate b :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> Left "Failed, but didn't crash!"
        Right True -> Right "*"

assertEqual :: Eq a => a -> a -> (a -> String) -> TestCase
assertEqual a c s =  do
    ret <- (try $ evaluate (a == c) :: IO (Either SomeException Bool))
    return $ case ret of
        Left ex -> Left $ "Failed with exception: " ++ (head $ lines $ show ex)
        Right False -> do Left $ intercalate "\n" ["Failed, but didn't crash!", "\t\tShould have been:" ++ (s c), "\t\tBut got:" ++ (s a)]
        Right True -> do Right "*"

assertError :: a -> String -> TestCase
assertError exp errMsg = do
    ret <- (try $ evaluate exp )
    return $ case ret of
        Left ex -> Right $ "\n\tPossibly passed, error message: " ++ (head $ lines $ show (ex :: SomeException))
        Right b -> Left $ "Failed, reason: " ++ errMsg

printResult :: Bool -> (Either String String)-> IO Bool
printResult _ (Left err) = putRed ("\n\t* "++ err) >> return False
printResult False (Right pass) = putGreen ("\n\t"++pass) >> return True
printResult True (Right pass) = putGreen pass >> return True

printTests :: Bool -> String -> [TestCase] -> [TestCase] -> IO Bool
printTests quiet label tests bonus = do
    if quiet then do
            results <- sequence tests
            let pass = all isRight results
            unless pass $
                do putStr (label++": ")
                   foldM_ printResult True $ filter isLeft results
            opts <- sequence bonus
            let passO = all isRight opts
            unless passO $
                do putStrLn (label++" non-critical:")
                   foldM_ printResult True $ filter isLeft opts
            return pass
        else do
            putStr (label++": ")
            results <- sequence tests
            foldM_ printResult True results
            putStrLn ""
            unless (null bonus) $
              do putStr "\tNon-critical tests (for full credit, no Prelude errors):"
                 opts <- sequence bonus
                 foldM_ printResult True opts
                 putStrLn ""
            return $ all isRight results

getRunTime :: Eq a => a -> IO NominalDiffTime
getRunTime expr = do
    start <- getCurrentTime
    let res = expr == expr
    unless res $ putStrLn "The world has ended. Please alert Dr. Fogarty."
    stop <- getCurrentTime
    let diff = diffUTCTime stop start
    when (diff < 0) $ putStr "The world has ended. Please alert Dr. Fogarty."
    return $ diff

assertTiming :: Eq a => a -> NominalDiffTime -> (String, String) -> IO (Either String String)
assertTiming expr timeout (fast, slow) = do
    runtime <- getRunTime expr
    if runtime > timeout
    then return $ Left $ unwords ["Failed, you are probably runing in",slow,"time instead of",fast,"time:", show runtime]
    else return $ Right $ unwords ["Passed! Your solution is probably running in",fast,"time", show runtime]


findBlowup :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> (Int -> Int) -> (Int -> Int) -> NominalDiffTime -> IO Double
findBlowup gen f n harden step threshold = aux n
    where aux :: Int -> IO Double
          aux n = do
            let caseN = gen n
            let caseSN = gen (step n)
            when (caseN /= caseN || caseSN /= caseSN) $ putStrLn "The world has ended. Please alert Dr. Fogarty."
            runtimeN <- getRunTime (f caseN)
            if (runtimeN < threshold)
            then aux (harden n)
            else do
              --putStrLn (show n)
              runtimeSN <- getRunTime (f caseSN)
              return ((realToFrac runtimeSN) / (realToFrac runtimeN))

assertLinear :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> IO (Either String String)
assertLinear gen f =
    do blowup <- findBlowup gen f 100 (2*) (2*) (0.5)
       if (blowup > 2.5)
       then return $ Left $ unwords
               ["Failed! Your solution is probably roughly quadratic. Doubling input size increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Right $ unwords
               ["Passed! Your solution is probably linear or n log(n). Doubling input size increased the runtime by a factor of", take 3 $ show blowup]


assertPoly :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> IO (Either String String)
assertPoly gen f start =
    do blowup <- findBlowup gen f start (1+) (1+) (0.3)
       if (blowup > 1.5)
       then return $ Left $ unwords
                ["Failed! Your solution is probably exponential. Increasing input by 1 increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Right $ unwords
                ["Passed! Your solution is probably polynomial. Increasing input by 1 increased the runtime by a factor of", take 3 $ show blowup]

assertNonPoly :: (Eq a, Eq b) => (Int -> a) -> (a -> b) -> Int -> IO (Either String String)
assertNonPoly gen f start =
    do blowup <- findBlowup gen f start (1+) (1+) (0.3)
       if (blowup > 1.5)
       then return $ Right $ unwords
                ["Passed! Your solution is probably exhaustive. Increasing input by 1 increased the runtime by a factor of ", take 3 $ show blowup]
       else return $ Left $ unwords
                ["Failed! Your solution is probably not exhaustive. Increasing input by 1 increased the runtime by a factor of", take 3 $ show blowup]

testGetKElems :: Bool -> IO Bool
testGetKElems q = printTests q "Testing getKElems"
        [ assertTrue $ 3 == length (getKElems 3 str)
        , assertTrue $ 0 == length (getKElems 0 str)
        , assertTrue $ 10 == (length $ nub $ getKElems 10 str)
        , assertTrue $ all (`elem` str) $ getKElems 20 str
        ] []
        where str = ['A'..'z']

testEucDist :: Bool -> IO Bool
testEucDist q = printTests q "Testing eucDist"
        [ assertTrue $ eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)
        , assertEqual (eucDist (0,0,10) (0,0,10)) 0.0 show
        , assertTrue $ (eucDist (0,0,10) (0,5.5,10) `elem` [5.5, 30.25])
        , assertTrue $ (round (eucDist (0,0,10) (5.34,5.28,10)) `elem` [56, 8])
        , assertTrue $ (eucDist (5,5,10) (0,0,10) > 0)
        ][]

testManhatDist :: Bool -> IO Bool
testManhatDist q = printTests q "Testing manhatDist"
        [ assertTrue $ manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)
        , assertEqual (manhatDist (0,0,10) (0,10,10)) 10.0 show
        , assertEqual (manhatDist (0,0,10) (10,0,10)) 10.0 show
        , assertEqual (manhatDist (0,0,10) (5.5,5.5,10)) 11.0 show
        , assertEqual (manhatDist (0,0,10) (5,4,10)) 9.0 show
        , assertEqual (manhatDist (0,5,10) (5,0,10)) 10.0 show
        , assertTrue $ (manhatDist (5,5,10) (0,0,10) > 0)
        ][]

testChebyDist :: Bool -> IO Bool
testChebyDist q = printTests q "Testing chebyDist"
        [ assertTrue $ chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)
        , assertEqual (chebyDist (15,0.5,10) (50,23,10)) 35.0 show
        , assertEqual (chebyDist (0,0,10) (55,0,10)) 55.0 show
        , assertEqual (chebyDist (15,15,10) (15,15,10)) 0.0 show
        , assertEqual (chebyDist (0,15,10) (15,0,10)) 15.0 show
        , assertEqual (chebyDist (0,10,10) (5,0,10)) 10.0 show
        , assertEqual (chebyDist (0,5,10) (10,0,10)) 10.0 show
        , assertTrue $ chebyDist (5,5,10) (0,0,10) > 0
        ][]

testTrafficDist :: Bool -> IO Bool
testTrafficDist q = printTests q "Testing trafficDist"
        [ assertTrue $ trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)
        , assertEqual (trafficDist (0,0,10) (0,10,10)) 10.0 show
        , assertEqual (trafficDist (0,0,10) (10,0,10)) 20.0 show
        , assertEqual (trafficDist (0,0,10) (5.5,5.5,10)) 16.5 show
        , assertEqual (trafficDist (0,0,10) (5,4,10)) 14.0 show
        , assertEqual (trafficDist (0,5,10) (5,0,10)) 15.0 show
        , assertTrue $ (trafficDist (5,5,10) (0,0,10) > 0)
        ][]

testTownshipDist :: Bool -> IO Bool
testTownshipDist q = printTests q "Testing townshipDist"
        [ assertTrue $ townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10)
        , assertEqual (townshipDist (0,0,10) (0,10,10)) 10.0 show
        , assertEqual (townshipDist (0,0,10) (10,0,20)) 20.0 show
        , assertEqual (townshipDist (0,0,10) (5.5,5.5,10)) 11.0 show
        , assertEqual (townshipDist (0,0,10) (5,4,20)) 18.0 show
        , assertEqual (townshipDist (0,5,10) (5,0,10)) 10.0 show
        , assertTrue $ (townshipDist (5,5,10) (0,0,20) > 0)
        ][]

testAverage :: Bool -> IO Bool
testAverage q = printTests q "Testing average"
        [ assertEqual (average [0,5,10]) 5.0 show
        , assertEqual ((fromIntegral $ round $ ((average $ [1..10] ++ [10,10,10]) * 100)) / 100) 6.54 show
        ][]

testMinimize :: Bool -> IO Bool
testMinimize q = printTests q "Testing minimize"
        [ assertEqual (minimize (fromIntegral . length) ["aaaa", "b"]) "b" show
        , assertEqual (minimize (abs) [-50..50]) 0.0 show
        , assertTrue $ length (minimize (fromIntegral . length) ["a", "c", "aa"]) == 1
        ][]

testBucket :: Bool -> IO Bool
testBucket q = printTests q "Testing bucket"
        [ assertEqual (sort $ bucket length [1..3] ["Hi","my","job","is","fun","!"]) [(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])] show
        , assertEqual (bucket length [] []) ([] :: [(Int,[String])]) show
        , assertEqual (sort $ bucket length [1..3] []) [(1,([]::[String])),(2,[]),(3,[])] show
        ][]

testAssignPoint :: Bool -> IO Bool
testAssignPoint q = printTests q "Testing assignPoint"
        [ assertEqual (assignPoint eucDist [(0,0,-1),(5,7,-1)] (5,0,100)) (0.0,0.0,-1) show
        , assertEqual (assignPoint trafficDist [(0,0,-1),(5,7,-1)] (5,0,100)) (5.0,7.0,-1) show
        , assertEqual (assignPoint manhatDist [(0,0,100),(2,0,200)] (5,0,100)) (2.0,0.0,200) show
        , assertEqual (assignPoint townshipDist [(0,0,100),(2,0,200)] (5,0,100)) (0.0,0.0,100) show
        ][]

sortClusters clusters = 
    sort $ map (\(a,b) -> (a,sort b)) clusters

testAnswer =  sortClusters $ [((1.0,1.0,-1),[(1.0,7.0,700)]),
                     ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),
                                    (7.5,3.5,500),(2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]
tightPoints = [(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400),(7.0,7.1,200),(7.5,7.5,500),
               (6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)]
tightCenters = [(1.0,1.0,100), (7,7,200)]
tightCentersEmpty = [(1.0,1.0,100),(0,0,300),(7,7,200)]
tightAnswer = sortClusters $ [((1.0,1.0,100),[(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400)]),
                        ((7.0,7.0,200),[(7.0,7.1,200),(7.5,7.5,500),(6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)])]
tightAnswerEmpty = ((0.0,0.0,300),[]):tightAnswer

testAssignPoints :: Bool -> IO Bool
testAssignPoints q = printTests q "Testing assignPoints"
        [ assertEqual (smry $ assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints) [((1.0,1.0,-1),1),((2.0,2.0,-1),9)] show
        , assertEqual (sortClusters $ assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints) testAnswer show
        , assertEqual (sortClusters $ assignPoints trafficDist tightCenters tightPoints) tightAnswer show
        , assertEqual (sortClusters $ assignPoints trafficDist tightCentersEmpty tightPoints) tightAnswerEmpty show
        ][]
     where smry clusters = [(c, length ps) | (c,ps) <- sortClusters clusters]

centersAreMeans ((x,y,l), points) = 
    let xs = map (\(x,_,_) -> x) points
        ys = map (\(_,y,_) -> y) points
    in x*(fromIntegral $ length xs) == sum xs &&  y * (fromIntegral $ length xs)  == sum ys

testFindMean :: Bool -> IO Bool
testFindMean q = printTests q "Testing findMean"
        [ assertEqual (findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)])) (Just (4.0,4.0,1)) show
        , assertEqual (findMean townshipDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)])) (Just (4.0,4.0,1)) show
        , assertEqual (findMean chebyDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])) (Just (5.0,5.0,0)) show
        , assertEqual (findMean eucDist ((3,3,9), [(0,2,9), (9.5,10,9), (3,3,0), (7.5,5,1)])) (Just (5.0,5.0,1)) show
        ][assertEqual (findMean townshipDist ((3,3,0), [])) Nothing show
        ]

testMoveCenters :: Bool -> IO Bool
testMoveCenters q = printTests q "Testing moveCenters"
        [ assertEqual (moveCenters trafficDist testAnswer) [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)] show
        , assertEqual (moveCenters trafficDist tightAnswer) tightMoved show
        ][assertEqual (moveCenters trafficDist tightAnswerEmpty) tightMoved show
        ]
        where tightMoved = [(1.0666666666666667,1.0666666666666667,300),(6.742857142857143,7.071428571428571,900)]
        

--use poitnsArClosest and kMeansProperties in onef ohese.
testImproveClusters :: Bool -> IO Bool
testImproveClusters q = printTests q "Testing improveClusters"
        [ assertEqual [(c, length ps) | (c,ps) <- newClusters] [((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)] show
        , assertEqual [(c, length ps) | (c,ps) <- newClusters] [((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)] show
        ][]
        where newClusters = improveClusters trafficDist 2 testAnswer

pointsAreClosest dist clusters = 
    let centers = map fst clusters
        pointDists = concatMap (\(c,ps) -> map (\p -> (p,dist p c)) ps) clusters
        smallestDist (p,d) = all (\c -> dist p c >= d) centers
    in all smallestDist pointDists

kMeansProperties dist k = 
    let clusters = kMeans dist k tenPoints
    in all centersAreMeans clusters && pointsAreClosest dist clusters


testKMeans :: Bool -> IO Bool
testKMeans q = printTests q "Testing kMeans"
        [ assertEqual (sortClusters $ kMeans eucDist 2 tenPoints) correctRes show
        , assertTrue $ kMeansProperties manhatDist 3
        , assertTrue $ kMeansProperties chebyDist 5
        , assertTrue $ kMeansProperties trafficDist 7
        ][]
      where correctRes = sortClusters $ [((7.5,2.1,400),sort [(7.0,1.0,100),(8.0,1.0,300),(8.0,2.0,400),(7.0,3.0,200),(7.5,3.5,500)])
                         ,((2.0,7.0,600),[(2.0,7.0,600),(1.0,7.0,700),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]

testGetKElemsSamples :: Bool -> IO Bool
testGetKElemsSamples q = printTests q "Testing getKElems samples evenly" 
        [ assertTrue $  "ABC" /= (getKElems 3 "ABCDEF")
        , assertTrue $ all testEnds [2..7]
        , assertEqual "AHP" (getKElems 3 ['A'..'P']) show
        , assertEqual "AFKP" (getKElems 4 ['A'..'P']) show
        , assertEqual "ADHLP" (getKElems 5 ['A'..'P']) show
        ][]
        where str = ['A'..'Z']
              testEnds k = let set = getKElems k ['A'..'P']
                          in ('A' `elem` set && 'P' `elem` set) 

testReplaceMissingClusters :: Bool -> IO Bool
testReplaceMissingClusters q = printTests q "Testing replacing missing clusters" 
        [ assertTrue $ length impClusters3 == 3
        , assertTrue $ length (filter upperRight impClusters3) == 2
        , assertEqual (upperRightPoints) (sort $ snd $ clusters!!1)  show
        ][]
      where clusters = [((1.0,1.0,100),[(1.1,1.0,100),(1.1,1.1,300),(1.0,1.1,400)]),
                        ((7.0,7.0,200),[(7.0,7.1,200),(7.5,7.5,500),(6.5,7.0,600),(6.0,7.0,700),(6.3,7.0,800),(6.9,7.0,900),(7.0,6.9,1000)])]
            impClusters3 = improveClusters eucDist 3 clusters
            upperRight ((x,y,l), ps) = x >= 6 && y >= 6
            upperRightPoints = sort $ concatMap snd (filter upperRight impClusters3)
            correctRes = sortClusters $ [((7.5,2.1,400),sort [(7.0,1.0,100),(8.0,1.0,300),(8.0,2.0,400),(7.0,3.0,200),(7.5,3.5,500)])
                         ,((2.0,7.0,600),[(2.0,7.0,600),(1.0,7.0,700),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


runTests :: Bool -> Bool -> IO ()
runTests q force = do
        milestone <- fmap and $ sequence $
                  [ testGetKElems q
                  , testEucDist q
                  , testManhatDist q
                  , testChebyDist q
                  , testTrafficDist q
                  , testTownshipDist q
                  , testAverage q
                  , testMinimize q
                  , testBucket q
                  ]
        if not milestone
        then putRedLn "Milestone not completed."
        else putGreenLn "You passed critical tests for the milestone.\n"
        when (milestone || force) $ do
        project <- fmap and $ sequence $
                  [ testAssignPoint q
                  , testAssignPoints q
                  , testFindMean  q
                  , testMoveCenters q
                  , testImproveClusters q
                  , testKMeans q
                  ]
        if not project
        then putRedLn "Project not completed."
        else putGreenLn "You pass all critical tests. Try running the project!\n"
        when (force || project) $ do
        fullCredit <- fmap and $ sequence $
                  [ testGetKElemsSamples  q
                  , testReplaceMissingClusters  q
                  ]
        if not fullCredit
        then putRedLn "Your project works, but is likely not worth full credit.\n"
        else putGreenLn "You pass all full-credit tests.\n"
