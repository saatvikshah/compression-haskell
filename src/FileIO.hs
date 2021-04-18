module FileIO where

import Control.Monad
import System.IO    

readFileContents :: String -> IO String
readFileContents file_arg = do
    file_handle <- openFile file_arg ReadMode
    hGetContents file_handle

writeContentToFile :: String -> String -> IO ()
writeContentToFile file_arg file_contents = do
    when (length file_contents > 0) $
            writeFile file_arg file_contents