module Main where

import System.Environment (getArgs)
import Huffman

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> recognizeFileEnding input output
        _               -> putStrLn "input output is needed!"

recognizeFileEnding :: FilePath -> FilePath -> IO ()
recognizeFileEnding input output = do
    if getFileEnding input == "comp"
        then decodeFile input output
        else encodeFile input output

getFileEnding :: String -> String
getFileEnding = reverse . takeWhile (/= '.') . reverse

