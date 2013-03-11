-- | Statistical functions on Integers.

module Stats where

import Data.Function
import Data.List (foldl', sort)

-- | Convert a Real to a Fractional
toFractional :: (Real a, Fractional b) => a -> b
toFractional = fromRational . toRational

-- | Remove duplicates from a sorted list.
nubSorted :: Eq a => [a] -> [a]
nubSorted [] = []
nubSorted [x] = [x]
nubSorted (x:xs@(y:ys))
    | x == y = nubSorted xs
    | otherwise = x : nubSorted xs

-- | Compute the average of a list.
average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = (toFractional $ foldl' (+) 0 xs) / (fromIntegral $ length xs)

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
standardDeviation :: (Real a, Floating b) => [a] -> b
standardDeviation xs =
    let avg = average xs
    in  sqrt . average . map (\x -> ((toFractional x) - avg)^2) $ xs

-- | Compute the standard deviation of a list.
-- Let u be the average of the list, then the standard deviation is
-- the sqrt(E[(X-u)^2]).
standardDeviation2 ::
    (Real a, Real b, Fractional b, Floating c) => [a] -> b -> c
standardDeviation2 xs avg =
    sqrt . average . map (\x -> ((toFractional x) - avg)^2) $ xs

-- | Map a list of elements into their standard units.
-- First we compute the standard deviation and scale each value
-- by its distance from the average
standardUnits :: (Real a, Floating b) => [a] -> [b]
standardUnits xs =
    let sd = standardDeviation xs
        avg = average xs
    in  map (\x -> ((toFractional x) - avg)/sd) xs

-- | Compute the covariance of two lists.
covariance :: (Real a, Floating b) => [a] -> [a] -> b
covariance xs ys =
    let avgx = average xs
        avgy = average ys
    in  average $ zipWith (*) (map (\x -> (toFractional x) - avgx) xs)
                              (map (\y -> (toFractional y) - avgy) ys)

-- | Compute the correlation between two lists.
correlation :: (Real a, Floating b) => [a] -> [a] -> b
correlation xs ys = (covariance xs ys) / (standardDeviation xs * standardDeviation ys)

    