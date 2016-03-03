#!/usr/bin/env runhaskell

import qualified Data.Char as Char
import qualified Data.Ord as Ord
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Text.Read as Read
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

main = interact getStats

getStats input =
  let allLines = map unspace $ lines input
      lineCount = length allLines
      fieldNames = Split.splitOn "," (head allLines)
      fieldNamesStripped = map stripFieldName fieldNames
      fieldTypes = map fieldNameToType fieldNames
      rows = map (Split.splitOn ",") (tail allLines)
      statsPerCol = getStatsFromRows fieldNames fieldTypes rows
      statsDisplayed = map display statsPerCol
      statsWithNames = zipWith (\ name stats -> name ++ "\n" ++ stats) fieldNamesStripped statsDisplayed
  in
  List.intercalate "\n\n" statsWithNames ++ "\n"

display :: Stats -> String
display NumberStats { count = count, nullCount = nullCount, least = least, most = most, total = total } =
  List.intercalate "\n" [" count:      " ++ show count,
                         " null count: " ++ show nullCount,
                         " min:        " ++ show least,
                         " max:        " ++ show most,
                         " average:    " ++ show avg]
  where avg = total / fromIntegral (count - nullCount)

display TextStats { stringToCount = stringToCount } =
  List.intercalate "\n" [" count:                 " ++ show count,
                         " null count:            " ++ show nullCount,
                         " count(shortest value): " ++ (show countShortest) ++ " " ++ shortestValue,
                         " count(longest value):  " ++ (show countLongest) ++ " " ++ longestValue,
                         " average length:        " ++ show avgLength]
  where count = sum (Map.elems stringToCount)
        nullCount = Map.findWithDefault 0 "" stringToCount
        countShortest = Map.findWithDefault 0 shortestValue stringToCount
        countLongest = Map.findWithDefault 0 longestValue stringToCount
        -- The sort here and for longestValue is to break ties alphabetically.
        shortestValue = head $ List.sort $ filter strIsOneOfTheShortest allStrings
        strIsOneOfTheShortest = \ str -> (not $ isNull str) && length str == shortestLength
        shortestLength = length $ head strsByLength
        longestValue = head $ List.sort $ filter strIsOneOfTheLongest allStrings
        strIsOneOfTheLongest = \ str -> length str == longestLength
        longestLength = length $ head $ reverse strsByLength
        strsByLength = List.sortOn length $ filter (not . isNull) $ Map.keys stringToCount
        avgLength = fromIntegral lengthTotal / (fromIntegral count - fromIntegral nullCount)
        lengthTotal = sum $ map (\(s, count) -> count * length s) stringToCountAsList
        allStrings = map Tuple.fst stringToCountAsList
        stringToCountAsList = Map.toList stringToCount

getStatsFromRows :: [String] -> [FieldType] -> [[String]] -> [Stats]
getStatsFromRows fieldNames fieldTypes rows =
  getStatsFromRows2 fieldNames fieldTypes rows initialStats
  where initialStats = map makeInitialStats fieldTypes

makeInitialStats :: FieldType -> Stats
makeInitialStats Number = NumberStats { count = 0, nullCount = 0, least = infinity, most = -infinity, total = 0 }
makeInitialStats Text = TextStats { stringToCount = Map.empty }

infinity :: Float
infinity = read "Infinity"

data Stats =
    NumberStats { count :: Int, nullCount :: Int, least :: Float, most :: Float, total :: Float }
  | TextStats { stringToCount :: Map.Map String Int }
  deriving Show

getStatsFromRows2 :: [String] -> [FieldType] -> [[String]] -> [Stats] -> [Stats]
getStatsFromRows2 _ _ [] statsAcc = statsAcc
getStatsFromRows2 fieldNames fieldTypes (row:rows) statsAcc =
  let statsAcc2 = zipWith3 updateStats fieldTypes row statsAcc in
  getStatsFromRows2 fieldNames fieldTypes rows statsAcc2

updateStats :: FieldType -> String -> Stats -> Stats
updateStats Number cell (NumberStats { count = count, nullCount = nullCount, least = least, most = most, total = total })  = 
  NumberStats { count = count + 1,
                nullCount = nullCount + if isNull cell then 1 else 0,
                least = maybe least (\num -> min least num) maybeNum,
                most = maybe most (\num -> max most num) maybeNum,
                total = maybe total (\num -> total + num) maybeNum }
  where maybeNum = Read.readMaybe cell
updateStats Text cell (TextStats { stringToCount = stringToCount }) =
  TextStats { stringToCount = Map.insertWith (+) cell 1 stringToCount }

isNull :: String -> Bool
isNull cell = cell == ""

-- |The unspace function removes all the spaces from a string.
unspace :: String -> String
unspace s = [c | c <- s, not(Char.isSpace c)]

data FieldType = Text | Number

fieldNameToType name =
  if "(number)" `List.isSubsequenceOf` name
    then Number
    else Text

-- |The stripFieldName function removes an assumed initial double quote and
-- everything from the left paren.
stripFieldName fieldName = drop 1 $ takeWhile (/= '(') fieldName

