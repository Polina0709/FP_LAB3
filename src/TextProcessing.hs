module TextProcessing
  ( normalizeSpaces
  , splitSentences
  , tokenize
  , swapFirstLast
  , processSentence
  , processText
  , lastCharIfPunct
  , dropFinalPunct
  ) where


import Data.Char (isAlpha)

-- Типи
type Symbol = Char
type WordCore = String
type Punctuation = String
type WordToken = (Punctuation, WordCore, Punctuation)
type Sentence = [WordToken]

------------------------------------------------------------
-- 1. Нормалізація пробілів і табуляцій
------------------------------------------------------------
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map (\c -> if c == '\t' then ' ' else c)


------------------------------------------------------------
-- 2. Розбиття на речення (. ! ?) зі збереженням пунктуації
------------------------------------------------------------
splitSentences :: String -> [String]
splitSentences [] = []
splitSentences xs =
  let (sent, rest) = break (`elem` ".!?") xs
  in case rest of
       []      -> [sent]
       (p:rs)  -> (sent ++ [p]) : splitSentences rs


------------------------------------------------------------
-- 3. Токенізація речення: (leading, core, trailing)
------------------------------------------------------------
tokenize :: String -> Sentence
tokenize = map splitPunct . words
  where
    splitPunct w =
      let leading  = takeWhile (not . isAlpha) w
          core     = takeWhile isAlpha (dropWhile (not . isAlpha) w)
          trailing = dropWhile isAlpha (dropWhile (not . isAlpha) w)
      in (leading, core, trailing)


------------------------------------------------------------
-- Додаткова утиліта: збирання токенів назад у рядок
------------------------------------------------------------
untokenize :: Sentence -> String
untokenize = unwords . map (\(l, c, r) -> l ++ c ++ r)


------------------------------------------------------------
-- 4. Обмін першого й останнього слова
------------------------------------------------------------
swapFirstLast :: Sentence -> Sentence
swapFirstLast []  = []
swapFirstLast [x] = [x]
swapFirstLast ws =
  let (l1, c1, r1) = head ws
      (l2, c2, r2) = last ws
  in  (l2, c2, r2) : (init (tail ws)) ++ [(l1, c1, r1)]


------------------------------------------------------------
-- 5. Обробка одного речення (без фінального пунктуаційного знака)
------------------------------------------------------------
processSentence :: String -> String
processSentence s =
  let core  = dropFinalPunct s
      punct = lastCharIfPunct s
      tokens = tokenize core
  in untokenize (swapFirstLast tokens) ++ punct

------------------------------------------------------------
-- 6. Допоміжні функції для роботи з фінальними . ! ?
------------------------------------------------------------

-- Видаляємо праві пробіли перед аналізом
trimRight :: String -> String
trimRight = reverse . dropWhile (== ' ') . reverse

lastCharIfPunct :: String -> String
lastCharIfPunct s =
  case reverse (trimRight s) of
    (c:_) | c `elem` ".!?" -> [c]
    _                      -> ""

dropFinalPunct :: String -> String
dropFinalPunct s =
  let t = trimRight s
  in case reverse t of
       (c:cs) | c `elem` ".!?" -> reverse cs
       _                       -> t

------------------------------------------------------------
-- 7. Повна обробка тексту
------------------------------------------------------------
processText :: String -> String
processText =
  unlines . map processSentence . filter (not . null)
  . splitSentences . normalizeSpaces
