module Main where

import TextProcessing (processText)

main :: IO ()
main = do
    input <- readFile "text.txt"
    let output = processText input
    writeFile "result.txt" output
    putStrLn "Готово! Результат записано у result.txt"
