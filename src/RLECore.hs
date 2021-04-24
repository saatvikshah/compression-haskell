module RLECore (runLengthEncode, runLengthDecode) where

import Data.Char (isNumber)
import Data.List (group)

data Encoding = Encoding Char Int

runLengthEncode :: String -> String
runLengthEncode = encodeRunLengths.encodeRunLengthGroups.group
    where
        encodeRunLengthGroups :: [String] -> [Encoding]
        encodeRunLengthGroups = map (\g -> Encoding (head g) (length g))
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