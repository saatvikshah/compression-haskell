module RLECore where

import Data.Char

runLengthEncode :: String -> String
runLengthEncode text = go '\0' 0 [] text
    where
        go :: Char -> Int -> String -> String -> String
        go ch ch_count result [] = case ch_count of
            0 -> result
            _ -> result ++ [ch] ++ show ch_count
        go ch ch_count result (x:xs)
            | x == ch       = go ch (ch_count+1) result xs
            | ch_count == 0 = go x 1 result xs
            | otherwise     = go x 1 (result ++ [ch] ++ show ch_count) xs

runLengthDecode :: String -> String
runLengthDecode cx = go '\0' 0 cx []
    where
        go :: Char -> Int -> String -> String -> String
        go ch ch_count [] result = case ch_count of
            0 -> result
            _ -> result ++ replicate ch_count ch
        go ch ch_count (x:xs) result
            | isNumber x = go ch (ch_count * 10 + digitToInt x) xs result
            | otherwise = go x 0 xs (result ++ replicate ch_count ch)