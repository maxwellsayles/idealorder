{-# OPTIONS_GHC -O -fglasgow-exts #-}

{-
The second largest prime factor is an indicator of the
primorial bound.  This program only looks at ideals that
lead to factorizations.  For each N, we compute the
min, the max, the average, and the standard deviation
of the second largest prime factors.
-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Text.Printf

import qualified IdealInfo
import qualified Primes
import qualified Stats
import qualified Mining


data Stat = Stat { statMin :: Integer,
                   statMax :: Integer,
                   statAvg :: Double,
                   statSd  :: Double } deriving Show

removeDupN :: [IdealInfo.IdealInfo] -> [IdealInfo.IdealInfo]
removeDupN [] = []
removeDupN [x] = [x]
removeDupN (x:y:ys)
    | IdealInfo.n x == IdealInfo.n y = x : removeDupN ys
    | otherwise = x : removeDupN (y:ys)

computeStat :: [Integer] -> Stat
computeStat xs =
    let maxX = foldl' max 0 xs
        minX = foldl' min maxX xs
        avgX = Stats.average xs
        sdX  = Stats.standardDeviation2 xs avgX
    in  Stat minX maxX avgX sdX

primorialPhiLookup :: Integer -> IO (Integer, Integer)
primorialPhiLookup prime =
    let filename = "primorials/" ++ show prime ++ ".txt"
    in  read <$> readFile filename

requiredPrimorialSteps :: Integer -> Integer -> IO Integer
requiredPrimorialSteps order largePrime = do
  (primorial, phi) <- primorialPhiLookup largePrime
  return $! Primes.requiredPrimorialSteps order primorial phi

appendStat :: String -> Stat -> Int -> IO ()
appendStat prefix stat nbits = do
  let minfile = prefix ++ "-min.dat"
  let maxfile = prefix ++ "-max.dat"
  let avgfile = prefix ++ "-avg.dat"
  let sdfile  = prefix ++ "-sd.dat"
  let mins = (nbits, statMin stat)
  let maxs = (nbits, statMax stat)
  let avgs = (nbits, statAvg stat)
  let sds  = (nbits, statSd  stat)
  Mining.appendGnuPlot minfile [mins]
  Mining.appendGnuPlot maxfile [maxs]
  Mining.appendGnuPlot avgfile [avgs]
  Mining.appendGnuPlot sdfile  [sds]

-- Return min, max, average, and standard deviation.
doStats :: Int -> Int -> Int -> IO ()
doStats nbits nmod4 k = do
  let infile = printf "ideals/ideal-%d.txt" nbits
  putStrLn $ "Infile: " ++ show infile
  putStrLn $ "Filtering by n=" ++ show nmod4 ++ " (mod 4) and k=" ++ show k

  xs <- map (take 2 . reverse . IdealInfo.factors) .
        removeDupN .
        filter ((>=2) . length . IdealInfo.factors) <$>
        IdealInfo.readIdealsFiltered infile (fromIntegral nmod4) k

  putStrLn $ "Found " ++ show (length xs) ++ " ideals."

  let statPrimes = computeStat $ map last xs
  putStrLn $ "prime: " ++ show statPrimes

  statSteps <- computeStat <$> mapM (\[x, y] -> requiredPrimorialSteps x y) xs
  putStrLn $ "steps: " ++ show statSteps

  putStrLn ""

  let primePrefix = printf "prime-%d-%d" nmod4 k
  appendStat primePrefix statPrimes nbits

  let stepsPrefix = printf "steps-%d-%d" nmod4 k
  appendStat stepsPrefix statSteps nbits

doFile :: Int -> IO ()
doFile nbits =
    sequence_ [doStats nbits nmod4 k | nmod4 <- [1, 3], k <- Mining.squareFree]

main :: IO ()
main = do
  mapM_ doFile [32::Int, 40..80]
--  let largestPrime = 911227
--  Primes.writePrimorialPhi largestPrime


