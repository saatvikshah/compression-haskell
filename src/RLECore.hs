module RLECore (runLengthEncode, runLengthDecode) where

import Data.Char (isNumber)

data Encoding = Encoding Char Int

runLengthEncode :: String -> String
runLengthEncode = encodeRunLengths.createRunLengths
    where
        createRunLengths :: String -> [Encoding]
        createRunLengths [] = []
        createRunLengths lst@(x:_) = Encoding x (length match) : createRunLengths remaining
            where
                (match, remaining) = span (==x) lst
        encodeRunLengths :: [Encoding] -> String
        encodeRunLengths = concatMap (\(Encoding ch ch_count) -> ch : show ch_count)

runLengthDecode :: String -> String
runLengthDecode = decodeAllEncodings.strToEncodings
    where
        strToEncodings :: String -> [Encoding]
        strToEncodings [] = []
        strToEncodings (ch:xs) = Encoding ch ch_count: strToEncodings remaining
            where
                ch_count = read ch_count_lst :: Int
                (ch_count_lst, remaining) = span isNumber xs
        decodeAllEncodings :: [Encoding] -> String
        decodeAllEncodings = concatMap (\(Encoding ch ch_count) -> replicate ch_count ch)