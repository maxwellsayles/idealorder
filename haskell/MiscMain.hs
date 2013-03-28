module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.IntMap as IntMap
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified IdealInfo as IdealInfo
import Mining
import qualified Primes as Primes
import Stats
import qualified System.Environment as System
import System.CPUTime
import Text.Printf

{--
main =
    do ideals <- readIdealRange [32..80]

       putStrLn "avgSuccessByPrime"
       print $ avgSuccessByPrime ideals
       putStrLn ""
                
       putStrLn "avgSuccessByMultiplier"
       print $ avgSuccessByMultiplier ideals
       putStrLn ""
--}

{-
-- for each multiplier, generate a graph of expected primes to nbits
main =
    do forM_ [32..80] $ \i ->
           do ideals <- readIdeals ("ideals/ideal-" ++ show i ++ ".txt")
              let exp = expectedPrimesByK ideals
              forM_ (IntMap.keys exp) $ \k ->
                  do let outfile = "exp-" ++ show k ++ ".dat"
                     let s = show i ++ ", " ++ show (exp IntMap.! k) ++ "\n"
                     appendFile outfile s
-}


-- for each multiplier, generate a graph of expected primes to nbits
-- separating n=1(mod4) and n=3(mod4)
expectedPrimes13 =
    do forM_ [32..80] $ \i ->
           do ideals <- IdealInfo.readIdeals ("ideals/ideal-" ++ show i ++ ".txt")

              let (ideals1, ideals3) = 
                      partition (\i -> (IdealInfo.n i) `mod` 4 == 1) ideals

              dump i ideals1 "exp1"
              dump i ideals3 "exp3"

    where dump i ideals fileprefix =
              do let exp = expectedPrimesByK ideals
                 forM_ (IntMap.keys exp) $ \k ->
                     do let outfile = fileprefix ++ "-" ++ show k ++ ".dat"
                        let s = show i ++ ", " ++ show (exp IntMap.! k) ++ "\n"
                        appendFile outfile s


histogramOfLogFactors =
    do ideals <- IdealInfo.readIdeals "ideals/ideal-80.txt"

       let factors = 
               map (\l -> (head l, length l)) .
               group .
               sort .
               foldr (++) [] .
               map (map (Primes.log2 . fromIntegral) . IdealInfo.factors) $
               ideals

       let output =
               unlines .
               map (\(x,y) -> show x ++ ", " ++ show y) $
               factors

       writeFile "hist-80.dat" output

{-
main =
    do ideals <- IdealInfo.readIdeals "ideals/ideal-32.txt"
       let factors = map (last . IdealInfo.factors) ideals
       let factors' = map fromIntegral factors
       print $ average factors'
       print $ standardDeviation factors'
-}
--main = expectedPrimes13
--    histogramOfLogFactors
      
-- Count the number of times p occurs in the factorization
-- of the order of ideals with n bits.
countPrimeInFactors n p =
    do ideals <- IdealInfo.readIdeals $ "ideals/ideal-" ++ show n ++ ".txt"
       let l = fromIntegral $ length ideals
       let xs = map (second $ (/l) . fromIntegral) $
                map (head &&& length) $
                groupBy (==) $ sort $
                map (length . filter (== p)) $
                map IdealInfo.factors ideals
       let m = fst $ maximumBy (compare `on` fst) xs
       let xs' = [(i, fromMaybe 0 $ lookup i xs) | i <- [0..m]]
       return xs'

timeLoading filename =
    do start <- getCPUTime
       i <- IdealInfo.readIdeals filename
       print . foldl' max 0 . map (last . IdealInfo.factors) $ i
       end <- getCPUTime
       print $ end - start

--main = timeLoading "/home/max/tmp/ideal-32.txt"

filterNK nmod4 k' = filter (\i -> IdealInfo.n i `mod` 4 == nmod4 && IdealInfo.k i == k')

cleanIdealFile infilename outfilename =
    sequence_ [f n k | n <- [1,3], k <- squareFree]
    where f n k =
              do putStrLn $ "Processing " ++ show n ++ ", " ++ show k
                 res <- IdealInfo.readIdealsFiltered infilename n k
                 let sortedRes = sortBy (compare `on` IdealInfo.n) res
                 let groupedRes = groupBy ((==) `on` IdealInfo.n) sortedRes
                 let groupedRes' = map (nubSorted . sortBy (compare `on` IdealInfo.p)) groupedRes
                 let filteredRes = filter ((==5) . length) groupedRes'
                 putStrLn $ show (length filteredRes) ++ " valid ideal groups."

{-
main = do
  ideals <- IdealInfo.readIdealsByN "/home/max/Desktop/ideals/ideal-32.txt"
  let validSets = filter ((==35) . Set.size) $
                  Map.elems ideals
  print $ length validSets
-}

{-
main = do
  i <- IdealInfo.readIdeals "/home/max/Desktop/ideals/ideal-32.txt"
  let ns = Set.toList $ foldl' (flip Set.insert) Set.empty $ map IdealInfo.n i
  forM_ ns $ \n ->
      do print n
         let i' = nubSorted . sort $ filter ((==n) . IdealInfo.n) i
         if length i' == 35
            then forM_ i' (appendFile "/home/max/tmp/ideal-32.txt" . (++"\n") . show)
            else return ()
-}

{-
main =
    cleanIdealFile "/home/max/Desktop/ideals/ideal-32.txt"
                   "/home/max/tmp/ideal-32"
-}
{-
main =
    do i <- IdealInfo.readIdeals "/home/max/Desktop/ideals/ideal-32.txt"
       let p = partitionMod4 i
       print $ length $ p !! 1
       print $ length $ p !! 3
--       forM_ p $ \i -> print $ avgIdealsUntilSuccessByK i
-}
{-
main =
    do forM_ [32,40..80] $ \i ->
           do xs <- countPrimeInFactors i 13
              let ys = map snd xs
              print xs
              print $ zip [0..] $ scanl1 (+) ys
              putStrLn ""
-}

{-
main = do
  ideals <- IdealInfo.readIdeals "/home/max/Desktop/ideals/ideal-32.txt"
  print $ length $ show $ nubSorted $ sort ideals
-}

{-
main = do
  let process :: Int -> IO ()
      process i = do
         putStrLn $ "Processing " ++ show i
         let filename = printf "/home/max/Desktop/masters/ideals/ideal-%d.txt" i
         ideals <- IdealInfo.readIdeals filename
         print $ length $
               filter ((>1) . length . filter (==11) . IdealInfo.factors) $
               ideals
         putStrLn ""
  mapM_ process [32,40..80]
-}

percentile i xs = let l = length xs
                  in  xs !! (l * i `div` 100)

moreThan1 :: [Int] -> Double
moreThan1 xs = let l = length xs
                   l' = length $ takeWhile (<=1) xs
               in  (fromIntegral l') / (fromIntegral l)

main = do
  let process :: Int -> IO ()
      process i = do
         putStrLn $ "Processing " ++ show i
         let filename = printf "/home/max/Desktop/masters/ideals/ideal-%d.txt" i
         ideals <- IdealInfo.readIdeals filename
--         print $ percentile 95 $ sort $
         print $ moreThan1 $ sort $
               map (length .
                    takeWhile (==11) .
                    dropWhile (<11) .
                    IdealInfo.factors) $
               ideals
         putStrLn ""
  mapM_ process [32,40..80]
