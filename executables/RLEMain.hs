module Main where

import FileIO
import RLECore
import System.Environment (getArgs)
import Data.List.Extra

processArgs :: [String] -> IO ()
processArgs args = case args of
    [] -> pure ()
    ("--compress":file_arg:xs) -> do
        file_contents <- readFileContents file_arg
        let compressed_content = runLengthEncode file_contents
        let compressed_fname = file_arg ++ ".compressed"
        writeContentToFile compressed_fname compressed_content
        processArgs xs
    ("--decompress":file_arg:xs) -> do
        let decompressed_fname = case stripSuffix ".compressed" file_arg of
                Just x  -> x
                Nothing -> error "Invalid input filename - should be suffixed with .compressed"
        file_contents <- readFileContents file_arg
        let decompressed_content = runLengthDecode file_contents
        writeContentToFile decompressed_fname decompressed_content
        processArgs xs
    _ -> error "Unrecognized arguments"

main :: IO ()
main = getArgs >>= processArgs
