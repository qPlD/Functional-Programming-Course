{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage
    ) where

import Text.HTML.Scalpel
import Data.Char
import Control.Exception
import Network.HTTP.Client
import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.Map.Strict as Map

-- Evaluate to IO Nothing when URL not found or no uncommon words
-- Otherwise return the most frequent word 
-- Ignore: stopwords.txt, single letter words, punctuation,'s, numbers, words starting with same 4 letters as title
-- All must be lowercase

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
  -- Extracting text as Maybe String, catching potential Http Exceptions
  textArray <- catch (extractText url) invalidHtttp
  title <- catch (extractTitle url) invalidHtttp
  
  let text = fromJust textArray
  let prefix = getPrefix title
  -- getting all the stopwords as a list of strings
  stopwords <- lines <$> readFile "stopwords.txt"

  --let removeS = unwords (map noApostrophes (words text))
  --let removeS = removeApo text---------------------------
  --let test = stripChars "0123456789" $ stripChars ",.?!—%*-–\\:[]();\"\'" $ stripChars "\n" removeS
  let test = stripChars "0123456789" $ stripChars "\n" text

  -- remove any character that is not a letter or a space from the string
  let lowercase = lowerString test
  let allwords = words lowercase
  --print test
  --print allwords
  let removeS = map noApostrophes allwords

  let removePunct = stripChars ",.?!—%*-–\\:[]();\"\'" (unwords removeS)
  let filteredWords = filterLenght $ words removePunct
  
  --let filtered = filter (not . stopwords) filteredWords
  let filtered = Data.List.filter (`notElem` stopwords) filteredWords
  let filterTitle = Data.List.filter (not . startswith prefix) filtered
  let filterOddBall = Data.List.filter (/= "\226\128\147") filterTitle
  
  let frequent = frequency filterOddBall
  let sorted = reverse $ sortBy (\(_,a) (_,b) -> compare a b) frequent
  --print sorted
  
  let mostFrequent = getFirst sorted
  --let result = unwords filtered
  --print filterOddBall
  --if (filterOddBall == [])-- || isNothing textArray)--mostFrequent isNothing || textArray == Nothing || title == Nothing)
    --then return Nothing
  --print title
  if (isNothing title)
    then return Nothing
    else return (Just mostFrequent)

lowerString str = [ toLower loweredString | loweredString <- str]

--removeApo :: String -> String
--removeApo = T.unpack . replace "\'s" "" . (T.pack)

noApostrophes :: String -> String
noApostrophes word 
    | endswith "'s" word        = init(init word)
    | otherwise                 = word

getFirst :: [(a,b)] -> a--Maybe a
--getFirst [] = Nothing
getFirst lst =  (fst (head (lst)))--Just (fst (head (lst)))

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

--extractText :: String -> IO (Maybe [URL])
--extractText (wikiUrl) = scrapeURL wikiUrl $ chroots anySelector $ text "p"

extractText :: URL -> IO (Maybe String)
extractText url = do
    maybeText <- scrapeURL url (text "body")
    let text = fromMaybe "" maybeText
    let lowerText = map toLower text
    return (Just lowerText)

extractTitle :: URL -> IO (Maybe String)
extractTitle url = do
  maybeTitle <- scrapeURL url (text "title")
  let title = fromMaybe "" maybeTitle
  let lowerTitle = map toLower title
  return (Just lowerTitle)

getPrefix :: Maybe String -> String
getPrefix title
    | isJust title = take 4 (fromJust title)
    | otherwise = ""

cleanString :: String -> String
cleanString xs = [ x | x <- xs, (not (isDigit x) && isAlpha x) || isSpace x ]

filterLenght :: [String] -> [String]
filterLenght xs = [ x | x <- xs, length(x) > 1]

-- Function to catch Http Exceptions when the url is invalid
invalidHtttp :: HttpException -> IO (Maybe String)
invalidHtttp ex = return Nothing
--toLowerStr :: Char -> Char
--toLowerStr xs = map toLower xs
--dropNonLetters xs = words $ (filter (\x -> x elem ['a'..'z']: ' ')) $ toLowerStr xs
--dropNonLetters :: [Char] -> [String]
--dropNonLetters xs = words $ (filter (\x -> x elem (' ':['a'..'z']))) $ toLowerStr xs

--strNoPunct map (stripChars ",.?!-:;\"\'") str
--stopwords <- getStopWords "../stopwords.txt"
--mapM_ putStrLn stopwords
  


  --mediawiki ltr sitedir-ltr mw-hide-empty-elt ns-0 ns-subject page-France rootpage-France skin-vector action-view
  --mw-parser-output

--filterStopWords :: [String] -> [String]
--filterStopWords words
--removePunc :: String -> String
--removePunc xs = x | x <- xs, not (x `elem` ",.?!-:;\"\'")
--removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'" :: Bool) ]
stripChars :: String -> String -> String
stripChars = Data.List.filter . flip notElem

--dropNonLetters xs = words $ (filter (\x -> x `elem` ['a'..'z'])) $ toLowerStr xs
--mostfrequentwordonpage :: URL -> IO (Maybe String)
--mostfrequentwordonpage page = do
--  stopwords <- getStopWords "../stopwords.txt"
--  mapM_ putStrLn stopwords
  
--  let scrapper url = scrapeURL url words
--  where
--    words :: Scraper String [String]
 --   words = texts $ "body"
  --let x = 11
  --let paragraphs = scrapeURL wikiUrl $ chroots anySelector $ text "p" <|> text "h3"
  --if x > 10
   -- then return (Just "Found")
   -- else return Nothing



getStopWords :: FilePath -> IO [String]
getStopWords filename = do contents <- readFile filename
                           return (lines contents)