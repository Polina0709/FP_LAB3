{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import TextProcessing

-- Великий текст для бенчмарку
largeSample :: String
largeSample = unlines (replicate 2000
  "Програмування — це творчий процес. Код повинен бути зрозумілим!")

main :: IO ()
main = defaultMain
  [ bench "normalizeSpaces" $ nf normalizeSpaces largeSample
  , bench "splitSentences"  $ nf splitSentences largeSample
  , bench "processText"     $ nf processText largeSample
  ]
