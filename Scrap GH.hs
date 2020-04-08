{-# LANGUAGE OverloadedStrings #-}

module Lib ( mostfrequentwordonpage ) where

import Text.HTML.Scalpel

import Control.Exception

import Data.Char                        -- toLower
import Data.List.Split                  -- splitOn
import qualified Data.Map.Strict as Map -- Word frequency map
import Data.Maybe                       -- The Maybe monad
import Data.String.Utils                -- startswith

import Network.HTTP.Client

-- Exposed function that returns the most frequent string
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
    badWords    <- extractStopwords "stopwords.txt"
    title       <- catch (extractTitle url) handleTimeout
    text        <- catch (extractText url) handleTimeout

    let validWordList   = createValidWordList title badWords text
    let wordFreq        = createWordFreq validWordList Map.empty
    let mostFreqWord    = getMostFreqWord wordFreq 
    
    if (validWordList == [] || wordFreq == Map.empty || mostFreqWord == "")
        then return Nothing
        else return (Just mostFreqWord)
    
handleTimeout :: HttpException -> IO (Maybe String)
handleTimeout ex = return Nothing

-- Private function that creates list of all words
createWordList :: Maybe String -> Maybe [String]
createWordList Nothing = Nothing
createWordList (Just text)  = Just (splitOn " " text)

-- Private function that creates list of all valid words
createValidWordList :: Maybe String -> [String] -> Maybe String -> [String]
createValidWordList title badwords text = validList
    where
        validList :: [String]
        validList = filter (checkWord title badwords) wordList
    
        wordList :: [String]
        wordList = map noApostrophes (fromMaybe [] (createWordList text))

        noApostrophes :: String -> String
        noApostrophes word 
            | endswith "'s" word        = init(init word)
            | otherwise                 = word

-- Private function that creates a word-frequency dict
createWordFreq :: [String] -> Map.Map String Int -> Map.Map String Int
createWordFreq wordList wordFreq
    | length wordList == 1 = Map.insertWith (+) (wordList!!0) 1 wordFreq
    | otherwise            = createWordFreq (tail wordList) (Map.insertWith (+) (head wordList) 1 wordFreq)
    
-- Private function that extracts the title of a page as text
extractTitle :: URL -> IO (Maybe String)
extractTitle url = do
    maybeTitle <- scrapeURL url (text "title")
    let title = fromMaybe "" maybeTitle
    let lowerTitle = map toLower title
    return (Just lowerTitle)

-- Private function that creates a stopwords list from a file
extractStopwords :: String -> IO [String]
extractStopwords filename = do
    contents <- readFile filename
    return (splitOn "\n" contents)

-- Private function to extract the body of the page, and convert it to pure text
extractText :: URL -> IO (Maybe String)
extractText url = do
    maybeText <- scrapeURL url (text "body")
    let text = fromMaybe "" maybeText
    let lowerText = map toLower text
    return (Just lowerText)

-- Private function that checks if the word should be included
checkWord :: Maybe String -> [String] -> String -> Bool
checkWord title badWords word = not (startswith prefix word || elem word badWords || length word <= 1 || notAlphOnly)
        where
            prefix :: String
            prefix = getPrefix title

            notAlphOnly :: Bool
            notAlphOnly = foldr (||) (False) (boolList)

            boolList :: [Bool]
            boolList = map isNotAlph word
            
            isNotAlph :: Char -> Bool
            isNotAlph c = not (c `elem` ("abcdefghijklmnopqrstuvwxyz" :: String))

-- Private function that gets the first 4 letters of the title (if there is one)
getPrefix :: Maybe String -> String
getPrefix title
    | isJust title = take 4 (fromJust title)
    | otherwise = ""

-- Private function that gets the word with the most occurences
getMostFreqWord :: Map.Map String Int -> String
getMostFreqWord wordFreq = do
    let words   = Map.keys wordFreq
    let freqs   = Map.elems wordFreq
    let maxFreq = foldr max 0 freqs
    (filter (checkMaxWord wordFreq maxFreq) words) !! 0

-- Private function that finds the word that has the highest frequency
checkMaxWord :: Map.Map String Int -> Int -> String -> Bool
checkMaxWord wordFreq maxFreq word = ((wordFreq Map.! word) == maxFreq)