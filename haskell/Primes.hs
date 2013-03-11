{- |

Stuff to deal with primes

-}

module Primes where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Map as Map

countSortedPrimes :: Ord a => [a] -> [(a, Int)]
countSortedPrimes = map (head &&& length) . group

-- | Simple eratosthenese prime sieve. Only useful for small primes.
primes :: [Integer]
primes = 2 : (filter (\x -> all (\p -> x `mod` p /= 0) $ 
                            takeWhile (\p -> p*p <= x) primes) 
              [3,5..])

-- | A primorial P_i is the product of all primes p \le p_i
primorials :: [Integer]
primorials = tail $ scanl (*) 1 primes

-- | The number of coprime values to P_i
phiPrimorials :: [Integer]
phiPrimorials = tail $ scanl (*) 1 $ map (\i -> i-1) primes

-- | Generates a (Primorial, Phi) lookup function.
-- i.e. Given a prime number, returns a function that takes a Prime number
-- and returns the Primorial and Phi associated with that Prime.
primorialPhiLookup :: Integer -> Integer -> (Integer, Integer)
primorialPhiLookup p =
    let (_, _, m) = foldl' f (1, 1, Map.empty) $ takeWhile (<=p) primes
        f (primorial, phi, map) prime =
            let primorial' = primorial * prime
                phi' = phi * (prime-1)
                map' = Map.insert prime (primorial', phi') map
            in  primorial' `seq` phi' `seq` map' `seq` (primorial', phi', map')
    in  (Map.!) m
--    let m = Map.fromList .
--            zip (takeWhile (<=p) primes) $
--            zip primorials phiPrimorials
--    in  (Map.!) m

writePrimorialPhi :: Integer -> IO ()
writePrimorialPhi p =
    foldM_ f (1, 1) $ takeWhile (<=p) primes
        where f (primorial, phi) prime = do
                let primorial' = primorial * prime
                let phi' = phi * (prime - 1)
                let fileName = "primorials/" ++ show prime ++ ".txt"
                let res = (primorial', phi')
                print prime
                writeFile fileName $ show res
                return res
                

-- | Inefficiently determine if a number is prime.
isPrime :: Integer -> Bool
isPrime x = elem x $ takeWhile (<=x) primes

-- | Inefficiently find the previous prime
prevPrime :: Integer -> Integer
prevPrime x = last $ takeWhile (<x) primes

-- | Inefficiently find the next prime
nextPrime :: Integer -> Integer
nextPrime x = head $ dropWhile (<=x) primes

-- | Computes the number of primorial steps necessary to find the
-- specified order given phi and the primorial:
-- => sqrt(order * phi / primorial)
requiredPrimorialSteps :: Integer -> Integer -> Integer -> Integer
requiredPrimorialSteps order primorial phi =
    ceiling $ sqrt $ fromIntegral $ (order * phi) `div` primorial

-- | Computes the largest exponent encountered
-- when a primorial step algorithm is run for 
-- the specified number of steps using the
-- specified primorial and phi.
-- => steps^2 * primorial / phi
largestPrimorialStep :: Integer -> Integer -> Integer -> Integer
largestPrimorialStep steps primorial phi =
    let steps' = fromIntegral steps :: Double
        primorial' = fromIntegral primorial :: Double
        phi' = fromIntegral phi :: Double
    in floor $ steps'^2 * primorial' / phi'

-- | Bits required to represent an integer.
log2 :: Integer -> Int
log2 n = loop n 0
    where loop 0 acc = acc
          loop 1 acc = 1 + acc
          loop n acc = loop (shift n (-1)) (1 + acc)
