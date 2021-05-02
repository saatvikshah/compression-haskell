{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module RLECore (runLengthEncode, runLengthDecode) where

import Data.Char (chr, ord)
import Data.ByteString.Char8 as C8 (pack)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.Text as T
import qualified Data.Vector as V

data RunLength = RunLength Char Int

-- https://stackoverflow.com/a/23833864/3656081
infixr 5 :<

pattern b :< bs <- (B.uncons -> Just (b, bs))
pattern Empty   <- (B.uncons -> Nothing)

charMax :: Int
charMax = 256

runLengthEncode :: T.Text -> B.ByteString 
runLengthEncode = C8.pack.V.toList.encodeRunLengths.createRunLengths.V.fromList.T.group
    where
        createRunLengths :: V.Vector T.Text -> V.Vector RunLength
        createRunLengths = V.concatMap createRunLengthGroup
            where
            createRunLengthGroup :: T.Text -> V.Vector RunLength
            createRunLengthGroup xs
                | xsLen <= maxRunLength = V.singleton $ RunLength ch xsLen
                | otherwise              = RunLength ch maxRunLength `V.cons` createRunLengthGroup xsRem
                    where
                        ch = T.head xs
                        xsLen = T.length xs
                        xsRem = T.drop maxRunLength xs
                        maxRunLength :: Int
                        maxRunLength = charMax - 1
        encodeRunLengths :: V.Vector RunLength -> V.Vector Char
        encodeRunLengths = V.concatMap (\(RunLength ch ch_count) -> V.fromList [ch, chr ch_count])

runLengthDecode :: B.ByteString -> Maybe T.Text
runLengthDecode xs = vectorToText.runLengthToString <$> inputToRunLengths xs
    where
        inputToRunLengths :: B.ByteString -> Maybe (V.Vector RunLength)
        inputToRunLengths Empty  = Just V.empty
        inputToRunLengths (ch :< encodedCharCount :< xsRem) = V.cons <$> Just (RunLength (BS.w2c ch) charCount) <*> inputToRunLengths xsRem
            where
                charCount = (ord.BS.w2c) encodedCharCount
        inputToRunLengths _ = Nothing
        runLengthToString :: V.Vector RunLength -> V.Vector Char
        runLengthToString = V.concatMap (\(RunLength ch charCount) -> V.replicate charCount ch)
        vectorToText :: V.Vector Char -> T.Text
        vectorToText = T.pack.V.toList