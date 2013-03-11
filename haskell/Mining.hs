
{-

Look just at successful ideals

Look if there's a multiplier or prime factor that works more often

If I use one 2 and an infinite number of primes raised to infinity, 
what ratio do i factor? Two 2s?  One 3? Two 3s? etc.
This way I know for a given primorial, assuming the search phase
is successful, we are successful how often?

Balance time of exponentiation with time of search.

-}


module Mining where

import Control.Applicative
import Control.Arrow
import Data.Function
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Map as Map

import IdealInfo
import Primes
import Stats

squareFree :: [Int]
squareFree = [1,2,3,5,6,7,10]

nbits :: IdealInfo -> Int
nbits = Primes.log2 . n

-- Return the number of ideals that led to a successful splitting
countSuccesses :: [IdealInfo] -> Int
countSuccesses = length . filter split

-- write a list of pairs out to a file for graphing with gnu plot
writeGnuPlot :: (Show a, Show b) => FilePath -> [(a, b)] -> IO ()
writeGnuPlot outfile points =
    do let showGnuPlot = unlines . map (\(x,y) -> (show x) ++ ", " ++ (show y))
       writeFile outfile $ showGnuPlot points

-- append a list of pairs out to a file for graphing with gnu plot
appendGnuPlot :: (Show a, Show b) => FilePath -> [(a, b)] -> IO ()
appendGnuPlot outfile points =
    do let showGnuPlot = unlines . map (\(x,y) -> (show x) ++ ", " ++ (show y))
       appendFile outfile $ showGnuPlot points

-- returns a list of the maximum number of times
-- a prime appears as a factor in the order of a qform
maxFactors :: [IdealInfo] -> [(Integer, Int)]
maxFactors =
    map (maximumBy (compare `on` snd)) .
    groupBy ((==) `on` fst) .
    sortBy (compare `on` fst) .
    foldr1 (++) .
    map (map (head &&& length)) .
    map (group . factors)

-- Returns the sum of all the factors
-- e.g. (2, x) where x is the total number of times 2 occurs as a factor
sumFactors :: [IdealInfo] -> [(Integer, Int)]
sumFactors =
    map (head &&& length) .
    group .
    sort .
    foldr1 (++) .
    map factors


-- returns the avg of all factors
-- e.g. (2, x) where x is the total number of 2s divided by the number of ideals
avgFactors :: [IdealInfo] -> [(Integer, Double)]
avgFactors infos =
    map (\(p,c) -> 
             let c' = fromIntegral c :: Double
             in  (p, c' / n')) .
    sumFactors $
    infos
    where n = length infos
          n' = fromIntegral n :: Double



firstPrimeNotFound :: String -> IO Integer
firstPrimeNotFound filename =
    do tmp <- readFile filename
       let ds = read tmp :: [(Integer, Int)]
       let ps = map fst ds
       let res = nextPrime . last . takeWhile (flip elem ps) $ primes
       return res


-- Returns a list of ideals where each one is uniq
uniqIdeals :: [IdealInfo] -> [IdealInfo]
uniqIdeals = nubSorted . sort


countUniqDiscriminants :: [IdealInfo] -> Int
countUniqDiscriminants = length . groupBy ((==) `on` n)
-- this is the proper way to do it, but our data happens to already be grouped
--countUniqDiscriminants = length . group . sort . map n


-- Return the ratio of the number of ideals to the number of successful factorings.
expectedPrimesGroupedByN :: [IdealInfo] -> Double
expectedPrimesGroupedByN =
    average .
    map (\l -> (fromIntegral $ length l) / (fromIntegral $ countSuccesses l)) .
    groupBy ((==) `on` n) .
    sortBy (compare `on` n)


expectedPrimesByK :: [IdealInfo] -> IntMap.IntMap Double
expectedPrimesByK =
    IntMap.map expectedPrimesGroupedByN .
    groupByK

-- Input is a list of ideals for a fixed k and fixed n
idealsUntilSuccess :: [IdealInfo] -> Int
idealsUntilSuccess = (+1) . length . takeWhile (not . split) . sortBy (compare `on` p)

-- Input is a list of ideals for a fixed k
avgIdealsUntilSuccess :: Fractional a => [IdealInfo] -> a
avgIdealsUntilSuccess =
    average .
    map fromIntegral .
    map idealsUntilSuccess .
    groupByN

avgIdealsUntilSuccessByK :: Fractional a => [IdealInfo] -> IntMap.IntMap a
avgIdealsUntilSuccessByK = IntMap.map avgIdealsUntilSuccess . groupByK

groupByN :: [IdealInfo] -> [[IdealInfo]]
groupByN = groupBy ((==) `on` n)

groupByK :: [IdealInfo] -> IntMap.IntMap [IdealInfo]
groupByK xs = IntMap.fromList .
              zip squareFree .
              map f $ squareFree
    where f i = filter ((==i) . k) xs

partitionMod4 :: [IdealInfo] -> [[IdealInfo]]
partitionMod4 xs = map f [0..3]
    where f i = filter ((==i) . flip mod 4 . n) xs

