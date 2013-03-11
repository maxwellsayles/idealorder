{-|
Compute avg, sd, max, and min
for both steps and prime by concatenating each
discriminant mod 4 and its multiplier.
-}

import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

readStats :: FilePath -> IO [Double]
readStats f = map read . map last . map words . lines <$> readFile f

files :: String -> String -> [FilePath]
files pre post = do
  m <- [1, 3] :: [Int]
  s <- [1, 2, 3, 5, 6, 7, 10] :: [Int]
  return $ printf "%s-%d-%d-%s.dat" pre m s post

avg :: String -> String -> IO [Double]
avg pre post = do
  let fs = files pre post
      n = fromIntegral $ length fs
  map (/n) . foldl1 (zipWith (+)) <$> mapM readStats fs

summarize :: String -> String -> FilePath -> IO ()
summarize pre post outfile =
    writeFile outfile .
        unlines .
        map (\(x, y) -> show x ++ ", " ++ show y) .
        zip [32 :: Int, 40..] =<<
        avg pre post

makeTriple :: String -> FilePath -> IO ()
makeTriple pre outfile = do
  avgs <- map last . map words . lines <$> readFile (pre ++ "-avg.dat")
  sds <- map last . map words . lines <$> readFile (pre ++ "-sd.dat")
  let res = zip3 (map show [32,40..]) avgs sds
  writeFile outfile . unlines .
      map (\(x, y, z) -> x ++ ", " ++ y ++ ", " ++ z) $ res

main = do
  summarize "prime" "avg" "prime-avg.dat"
  summarize "prime" "sd"  "prime-sd.dat"
  summarize "steps" "avg" "steps-avg.dat"
  summarize "steps" "sd"  "steps-sd.dat"
