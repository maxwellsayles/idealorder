import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified System as System

usage = do
  progName <- System.getProgName
  putStrLn $ "Usage: " ++ progName ++ " <input> <output>"

firstInteger = fst . fromJust . BS.readInteger
    

doSort = do
  args <- System.getArgs
  let infile = args !! 0
  let outfile = args !! 1

  BS.writeFile outfile .
    BS.unlines .
    join .
    Map.elems .
    Map.fromList .
    map ((fst . head) &&& map snd) .
    groupBy ((<=) `on` fst) .
    map (firstInteger &&& id) .
    BS.lines =<<
    BS.readFile infile

main = do
  args <- System.getArgs
  if length args /= 2 then usage else doSort
