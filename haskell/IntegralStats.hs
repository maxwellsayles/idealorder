-- | Statistical functions on Integers.

module IntegralStats where

import Data.List (foldl', sort)

-- | Integer square root.
-- Use a binary search to find the largest integer x
-- such that x^2 <= n.
integralSqrt :: Integral a => a -> a
integralSqrt n = f 0 $ n `div` 2
    where f l u | l == u = l
                | l == u-1 = if u*u <= n then u else l
                | otherwise = let m = ((u-l) `div` 2) + l
                                  m2 = m*m
                                  l' = if m2 <= n then m else l
                                  u' = if m2 >= n then m else u
                              in  f l' u'

-- | Compute the average of a list.
average :: Integral a => [a] -> a
average [] = 0
average xs = (foldl' (+) 0 xs) `div` (fromIntegral $ length xs)

-- | The median value of an arbitrary (unsorted) list.
-- Requires O(nlogn) for sorting.
median :: Ord a => [a] -> a
median xs = (sort xs) !! (length xs `div` 2)

-- | The median value of a sorted list, i.e. the middle element.
medianSorted :: [a] -> a
medianSorted xs = xs !! (length xs `div` 2)

-- | Compute the standard deviation of a list.
-- Let u be the average of the list, then the standard deviation is
-- the sqrt(E[(X-u)^2]).
standardDeviation :: Integral a => [a] -> a
standardDeviation xs =
    let avg = average xs
    in  integralSqrt . average . map (\x -> (x - avg)^2) $ xs

-- | Compute the standard deviation of a list given the average.
-- Let u be the average of the list, then the standard deviation is
-- the sqrt(E[(X-u)^2]).
standardDeviation2 :: Integral a => [a] -> a -> a
standardDeviation2 xs avg =
    integralSqrt . average . map (\x -> (x - avg)^2) $ xs

-- | Map a list of elements into their standard units.
-- First we compute the standard deviation and scale each value
-- by its distance from the average
standardUnits :: Integral a => [a] -> [a]
standardUnits xs =
    let sd = standardDeviation xs
        avg = average xs
    in  map (\x -> (x - avg) `div` sd) xs

-- | Compute the covariance of two lists.
covariance :: Integral a => [a] -> [a] -> a
covariance xs ys =
    let avgx = average xs
        avgy = average ys
    in  average $ zipWith (*) (map (\x -> x - avgx) xs)
                              (map (\y -> y - avgy) ys)

-- | Compute the correlation between two lists.
correlation :: Integral a => [a] -> [a] -> a
correlation xs ys =
    (covariance xs ys) `div` (standardDeviation xs * standardDeviation ys)

    