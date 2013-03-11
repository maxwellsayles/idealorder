{-|
This program will sanitize a text file of ideals, their order,
and their factorisation.  For each n, we expect 35 unique entries.
Any value of n that doesn't have 35 unique entries is removed.
-}

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function
import Data.List
import qualified Data.Set as Set
import qualified IdealInfo as IdealInfo
import qualified System as System

validSetSize = 35

usage = do
  progName <- System.getProgName
  putStrLn $ "Usage: " ++ progName ++ " <input> <output>"

sanitize = do
  args <- System.getArgs
  let infile = args !! 0
  let outfile = args !! 1
  ideals <- IdealInfo.readIdeals infile
  let g = concat .
          filter ((== validSetSize) . length) .
          map (Set.toList . Set.fromList) .
          groupBy ((==) `on` IdealInfo.n) $ ideals
  BS.writeFile outfile .
      BS.unlines .
      map (BS.pack . show) $ g

main = do
  args <- System.getArgs
  if length args /= 2 then usage else sanitize
