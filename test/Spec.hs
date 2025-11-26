module Main where

import Test.Hspec
import Test.QuickCheck
import TextProcessing
import Data.List (isInfixOf)
import Data.Char (isAlpha)

main :: IO ()
main = hspec $ do

  ----------------------------------------------------------
  -- ЮНІТ-ТЕСТИ
  ----------------------------------------------------------

  describe "normalizeSpaces" $ do
    it "replaces tabs with spaces and collapses multiple spaces" $
      normalizeSpaces "Hello\t   world" `shouldBe` "Hello world"

  describe "splitSentences" $ do
    it "splits simple punctuation" $
      splitSentences "Hi! How are you? Fine." `shouldBe`
        ["Hi!", " How are you?", " Fine."]

  describe "tokenize" $ do
    it "keeps punctuation correctly" $
      tokenize "Привіт, світе!" `shouldBe`
        [("", "Привіт", ","), ("", "світе", "!")]

  describe "swapFirstLast" $ do
    it "swaps" $
      swapFirstLast [("", "A", ""), ("", "B", ""), ("", "C", "")]
        `shouldBe` [("", "C", ""), ("", "B", ""), ("", "A", "")]

  describe "processSentence" $ do
    it "preserves punctuation" $
      processSentence "Програмування — це творчий процес!"
        `shouldBe` "процес — це творчий Програмування!"

  ----------------------------------------------------------
  -- PROPERTY-ТЕСТИ
  ----------------------------------------------------------

  describe "Property tests" $ do

    it "normalizeSpaces never produces consecutive spaces" $
      property $ \xs ->
        not ("  " `isInfixOf` normalizeSpaces xs)

    it "processing preserves final punctuation (for sentences with words)" $
      property $ \s ->
        any isAlpha s ==>        -- only valid sentences
          let punct = lastCharIfPunct s
          in lastCharIfPunct (processSentence s) == punct

    it "swapFirstLast is involutive for 2-word sentences" $
      property $ \(NonEmpty ws') ->
        let ws = map (\w -> ("", w, "")) (take 2 ws')
            once = swapFirstLast ws
            twice = swapFirstLast once
        in ws == twice

    it "processing does not change number of words" $
      property $ \s ->
        let before = length (words (dropFinalPunct s))
            after  = length (words (dropFinalPunct (processSentence s)))
        in before == after
