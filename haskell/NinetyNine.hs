{- |
For the smallest odd primes, computes the exponent of each prime in the
factorization of the order of an ideal.  The list of exponents is then
sorted and the value such that 99% of all exponents is less than or equal to
is selected.  The list is then traversed again to determine the probability
that the exponent in the order is <= to the value at the 99th percentile.

This generates "ninety-nine.dat" representing this information.
-}

import Data.List
import Text.Printf

import qualified IdealInfo as IdealInfo

percentile i xs = let l = length xs
                  in  xs !! (l * i `div` 100)

main = do
  let probLe :: Int -> [Int] -> Double
      probLe i xs = let l = length xs
                        l' = length $ takeWhile (<= i) xs
                    in  (fromIntegral l') / (fromIntegral l)

  let processFile :: Int -> IO ()
      processFile i = do
         printf "Processing: i=%d\n" i
         let filename =
                 printf "/home/max/Desktop/masters/ideals/ideal-%d.txt" i
         
         let processPrime :: Integer -> IO (Int, Double)
             processPrime p =
                 do ideals <- IdealInfo.readIdeals filename
                    let vs = sort .
                             map (length .
                                  takeWhile (== p) .
                                  dropWhile (< p) .
                                  IdealInfo.factors) $
                             ideals
                    let x = percentile 99 vs
                    let y = probLe x vs
                    printf "p=%d, %d gives %.5f probability\n\n" p x y
                    return (x, y)
                           
         rows <- mapM processPrime [3, 5, 7, 11, 13, 17]
         appendFile "99-percentile.dat" .
                    (\s -> show i ++ ", " ++ s ++ "\n") .
                    intercalate ", " .
                    map (\(x, y) -> printf "%d, %.5f" x y) $
                    rows

  mapM_ processFile [32, 40 .. 80]