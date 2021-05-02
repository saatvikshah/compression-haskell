module Main where

import RLECore
import Options.Applicative
import Data.List.Extra
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B

data Args = Args
    {   argsCompress   :: Maybe String,
        argsDecompress :: Maybe String
    }

parseArgs :: Parser Args
parseArgs = Args
    <$> (optional $ strOption
        $  long "compress"
        <> short 'c'
        <> help "file to be compressed")
    <*> (optional $ strOption
        $   long "decompress"
        <>  short 'd'
        <>  help "file to be decompressed")

processArgs :: Args -> IO ()
processArgs (Args (Just file_arg) Nothing) = do
    file_contents <- TIO.readFile file_arg
    let compressed_content = runLengthEncode file_contents
    let compressed_fname = file_arg ++ ".compressed"
    B.writeFile compressed_fname compressed_content
processArgs (Args Nothing (Just file_arg)) = do    
    let decompressed_fname = case stripSuffix ".compressed" file_arg of
            Just x  -> x
            Nothing -> error "Invalid input filename - should be suffixed with .compressed"
    file_contents <- B.readFile file_arg
    let decompressed_content = case runLengthDecode file_contents of
            Just x  -> x
            Nothing -> error "Corrupt input file"
    TIO.writeFile decompressed_fname decompressed_content
processArgs _ = return ()

main :: IO ()
main = execParser opts >>= processArgs
    where
        opts = info (parseArgs <**> helper)
            (   fullDesc
            <>  progDesc "Compress or decompress a file using run-length encoding."
            <>  header "rle-compressor" )
