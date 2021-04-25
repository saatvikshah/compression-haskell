module RLECore (runLengthEncode, runLengthDecode) where

import Data.Char (chr, ord)
import Data.List (group)

data RunLength = RunLength Char Int

charMax :: Int
charMax = 256

runLengthEncode :: String -> String
runLengthEncode = encodeRunLengths.createRunLengths.group
    where
        createRunLengths :: [String] -> [RunLength]
        createRunLengths = concatMap createRunLengthGroup
            where
            createRunLengthGroup :: String -> [RunLength]
            createRunLengthGroup xs
                | xs_len <= maxRunLength = [RunLength ch xs_len]
                | otherwise              = (RunLength ch maxRunLength): createRunLengthGroup xs_rem
                    where
                        ch = head xs
                        xs_len = length xs
                        xs_rem = drop maxRunLength xs
                        maxRunLength :: Int
                        maxRunLength = charMax - 1
        encodeRunLengths :: [RunLength] -> String
        encodeRunLengths = concatMap (\(RunLength ch ch_count) -> [ch, chr ch_count])

runLengthDecode :: String -> String
runLengthDecode = runLengthToString.inputToRunLengths
    where
        inputToRunLengths :: String -> [RunLength]
        inputToRunLengths [] = []
        inputToRunLengths (ch:encoded_ch_count:xs) = RunLength ch ch_count: inputToRunLengths xs
            where
                ch_count = ord encoded_ch_count
        runLengthToString :: [RunLength] -> String
        runLengthToString = concatMap (\(RunLength ch ch_count) -> replicate ch_count ch)