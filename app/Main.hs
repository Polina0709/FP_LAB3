module Main where

import Data.Char (isAlpha, isSpace)

-- Типи
type Symbol = Char
type WordCore = String
type Punctuation = String
type WordToken = (Punctuation, WordCore, Punctuation)
type Sentence = [WordToken]

-- Нормалізація пробілів
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map (\c -> if c == '\t' then ' ' else c)

-- Розбиття тексту на речення, але збереження знаків . ! ?
splitSentences :: String -> [String]
splitSentences [] = []
splitSentences xs = let (sent, rest) = break (`elem` ".!?") xs
                    in case rest of
                         []     -> [sent]
                         (p:rs) -> (sent ++ [p]) : splitSentences rs

-- Розбиваємо речення на токени зі збереженням пунктуації
tokenize :: String -> Sentence
tokenize = map splitPunct . words
  where
    splitPunct w =
      let leading = takeWhile (not . isAlpha) w
          core    = takeWhile isAlpha (dropWhile (not . isAlpha) w)
          trailing = dropWhile isAlpha (dropWhile (not . isAlpha) w)
      in (leading, core, trailing)

-- Склеювання назад
untokenize :: Sentence -> String
untokenize = unwords . map (\(l,c,r) -> l ++ c ++ r)

-- Міняємо місцями тільки core слів, пунктуація лишається
swapFirstLast :: Sentence -> Sentence
swapFirstLast [] = []
swapFirstLast [x] = [x]
swapFirstLast ws =
    let (l1,c1,r1) = head ws
        (l2,c2,r2) = last ws
    in (l2,c2,r2) : (init (tail ws)) ++ [(l1,c1,r1)]

-- Обробка речення цілком
processSentence :: String -> String
processSentence s = untokenize $ swapFirstLast $ tokenize s

-- Повна обробка тексту
processText :: String -> String
processText = unlines . map processSentence . filter (not . null) . splitSentences . normalizeSpaces

main :: IO ()
main = do
    text <- readFile "text.txt"
    let result = processText text
    writeFile "result.txt" result
    putStrLn "✅ Обробка завершена! Перевірте result.txt"
