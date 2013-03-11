{- |
Describes ideals and their relationship to factoring a discriminant.
-}

module IdealInfo (IdealInfo, 
                  n, 
                  k, 
                  p, 
                  order, 
                  split, 
                  factors, 
                  readIdeals, 
                  readIdealRange,
                  readIdealsFiltered)
where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

data IdealInfo = IdealInfo { n :: Integer,
                             k :: Int,
                             p :: Int,
                             order :: Integer,
                             split :: Bool,
                             factors :: [Integer] }

instance Eq IdealInfo where
    (IdealInfo n1 k1 p1 _ _ _) == (IdealInfo n2 k2 p2 _ _ _) =
        n1 == n2 && k1 == k2 && p1 == p2
    (IdealInfo n1 k1 p1 _ _ _) /= (IdealInfo n2 k2 p2 _ _ _) =
        n1 /= n2 || k1 /= k1 || p1 /= p2

instance Ord IdealInfo where
    (IdealInfo n1 k1 p1 _ _ _) `compare` (IdealInfo n2 k2 p2 _ _ _)
        | n1 < n2 = LT
        | n1 > n2 = GT
        | k1 < k2 = LT
        | k1 > k2 = GT
        | p1 < p2 = LT
        | p1 > p2 = GT
        | otherwise = EQ

instance Show IdealInfo where
    show (IdealInfo n k p order split factors) =
        show n ++ " " ++
        show k ++ " " ++
        show p ++ " " ++
        show order ++ " " ++
        (if split then "1" else "0") ++ " " ++
        show factors

-- Read all ideal info from a Pari/GP file
readIdeals :: FilePath -> IO [IdealInfo]
readIdeals filename =
    map makeIdealInfo . ByteString.lines <$> ByteString.readFile filename

makeIdealInfo :: ByteString.ByteString -> IdealInfo
makeIdealInfo bs =
    let Just (n, bs1) = ByteString.readInteger bs
        Just (k, bs2) = ByteString.readInt $! ByteString.tail bs1
        Just (p, bs3) = ByteString.readInt $! ByteString.tail bs2
        Just (order, bs4) = ByteString.readInteger $! ByteString.tail bs3
        Just (split, bs5) = ByteString.readInt $! ByteString.tail bs4
        factors = read . ByteString.unpack $! ByteString.tail bs5
    in  IdealInfo n k p order (split == 1) factors
        
-- Reads all the ideals in a range into one large list
readIdealRange :: [Int] -> IO [IdealInfo]
readIdealRange range =
    concat <$> mapM (\i -> readIdeals ("ideals/ideal-" ++ show i ++ ".txt")) range

-- Read ideals from a file and filter by n (mod 4) and k
readIdealsFiltered :: String -> Integer -> Int -> IO [IdealInfo]
readIdealsFiltered filename nmod4 k' =
    filter (\i -> k i == k' && n i `mod` 4 == nmod4) <$> readIdeals filename
