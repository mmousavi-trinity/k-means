module Main where
import Data.Char
import Data.List
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.Exit
import Coloring
import Cluster
import ClusterData
import System.Drawille
import Testing

-- Options record
data Options = Options {
   optHelp              :: Bool
 , optTest              :: Bool
 , optQuiet             :: Bool
 , optForceTests        :: Bool
 , optIncremental       :: Bool
 , optVerbose           :: Bool
 , optDisplay           :: Bool
 , scaleF               :: ([Cluster] -> [Cluster])
 , pointCount           :: Int
 , metric               :: (Point -> Center -> Double)
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optTest = False
    , optQuiet = False
    , optForceTests = False
    , optIncremental = False
    , optVerbose = False
    , scaleF  = id
    , pointCount = 10
    , optDisplay = False
    , metric = eucDist
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]        (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['n'] []              (ReqArg (\n opts -> opts { pointCount = (read n) }) "n") "Use n points.",
  Option ['d'] ["display"]     (NoArg  (\opts -> opts { optDisplay = True })) "Render the points and their clusters on the screen.",
  Option ['i'] ["incremental"] (NoArg  (\opts -> opts { optIncremental = True })) "Output step-by-step as the algorithm runs.",
  Option ['v'] ["verbose"]     (NoArg  (\opts -> opts { optVerbose = True })) "Display more information during processing.",
  Option ['m'] ["metric"]      (ReqArg (\m opts -> opts { metric = (readMetric m) }) "m") ("Set the distance metric.\n" ++ showMetricOpts),
  Option ['q'] ["quiet"]       (NoArg  (\opts -> opts { optQuiet = True})) "Only print error messages on tests, or minimal output when solving.",
  Option [] ["forceTests"]       (NoArg  (\opts -> opts { optTest=True, optForceTests = True})) "Force evaluation of all test cases, regardless of early failures.",
  Option ['t'] ["test"]        (NoArg  (\opts -> opts { optTest = True })) "Runs a series of tests on your code"
  ]

showMetricOpts :: String
showMetricOpts = "\tOptions:"
              ++ "\n\t\teuclid\t- Euclidian Distance"
              ++ "\n\t\tmanhat\t- Manhattan Distance"
              ++ "\n\t\tcheby\t- Chebyshev Distance"
              ++ "\n\t\ttraffic\t- Traffic Distance"
              ++ "\n\t\tregion\t- Township Distance"

readMetric :: String -> (Point -> Center -> Double)
readMetric "euclid" = eucDist
readMetric "manhat" = manhatDist
readMetric "cheby"  = chebyDist
readMetric "traffic" = trafficDist
readMetric "region" = townshipDist
readMetric s = error "Invalid metric"

-- Return the list of flags
compilerOpts :: [String] -> (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: cluster [OPTION]... [HAND]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./cluster [OPTION]... [k] [file]"

-- Main IO function
main :: IO ()
main = do
  allArgs <- getArgs
  let (opts, args) = compilerOpts allArgs
  when (length args > 2) $ do
         putStrLn "Error: more than two arguments. Ignoring all but the first two."
  case (optHelp opts, optTest opts, args) of
    (True, _,_) -> helpIO
    (_, True,_) -> runTests (optQuiet opts) (optForceTests opts)
    (_,_,[])    -> do
                     points <- getKRandomPoints (pointCount opts)
                     playKMeans opts 2 points
    (_,_,[k])   -> do
                    points <- getKRandomPoints (pointCount opts)
                    playKMeans opts (read k) points
    (_,_,[k,fl])-> do
                    points <- fmap fst $ pointsOfFile (pointCount opts) fl
                    playKMeans opts {scaleF=scaleClusters} (read k) points
    _ -> error "Improper arguments. Usage: ./cluster [OPTION]... [k] [file]"


-- kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
playKMeans :: Options -> Int -> [Point] -> IO ()
playKMeans opts k points =
  if optIncremental opts
    then playIncrementalKMeans opts k points
    else do
      let clusters = kMeans (metric opts) k points
      if (optDisplay opts)
        then (showClustering . scaleF opts) clusters
        else basicDisplay (optVerbose opts) clusters

-- one step at a time
playIncrementalKMeans :: Options -> Int -> [Point] -> IO ()
playIncrementalKMeans opts k points = 
  let initCenters = getKElems k (nub points)
      initClusters = assignPoints (metric opts) initCenters points
      showFunc = if (optDisplay opts) then (showClustering . scaleF opts) else basicDisplay (optVerbose opts)
      iterFunc = improveClusters (metric opts) k
      increment i clusters = do
        putMagentaLn $ "Iterations: " ++ (show i)
        showFunc clusters
        putMagentaLn $ "Hit \"Enter\" to proceed, or type \"q\" to quit."
        input <- getLine
        case input of
            "q" -> putGreenLn "Exiting!" >> exitSuccess
            _   -> increment (i+1) (iterFunc clusters)
  in increment 0 initClusters
  

basicDisplay :: Bool -> [Cluster] -> IO ()
basicDisplay verbose clusters = aux 1 clusters
  where
    showPoint (x,y,l) =
      let x' = (fromInteger $ round $ x * (10^2)) / (10.0^^2)
          y' = (fromInteger $ round $ y * (10^2)) / (10.0^^2)
      in  show (x', y')
    aux n ((c, ps):xs) = do
        putStr $ "Center " ++ (show n) ++ " " ++ showPoint c ++ " covering " ++ (show (length ps)) ++ " points"
        when verbose $ putStr $ ": "++ (unwords $ map showPoint ps)
        putStrLn "."
        aux (n + 1) xs
    aux _ [] = return ()

getKRandomPoints :: Int -> IO [Point]
getKRandomPoints k = do
  ps <- replicateM (k * 2) $ randomRIO(0.0,10.0) :: IO [Double]
  let rounded = map (\x -> fromIntegral (round (x * 100)) / 100.0) $ ps
  let pairs = pair 0 rounded
  return pairs
  where
    pair n (x:y:xs) = (x,y,n):(pair (n + 100) xs)
    pair _ _ = []

-- (Center, [Point])
showClustering :: [Cluster] -> IO ()
showClustering c = do
  let centers = highlight $ map (\(a, b, _) -> (round (a * 10), round (b * 10))) $ map fst c
      points  = map (\(a, b, _) -> (round (a * 10), round (b * 10))) $ foldl (++) [] $ map snd c
      plot    = frame $ fromList $ centers ++ points
  putWhiteLn $ plot

-- create visual clusters by expanding their points into squares
highlight :: [(Int, Int)] -> [(Int, Int)]
highlight (p@(x, y):xs) = p : (x - 1, y) : (x, y - 1) : (x + 1, y) : (x, y + 1) : (x + 1, y + 1) : (x + 1, y - 1) : (x - 1, y + 1) : (x - 1, y - 1) : highlight xs
highlight [] = []
