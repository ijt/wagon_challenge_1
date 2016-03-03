#!/usr/bin/env runhaskell

import qualified Data.Char as Char
import qualified Data.Ord as Ord
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Text.Read as Read

main = interact getStats

getStats input =
  let allLines = map unspace $ lines input
      lineCount = length allLines
      fieldNames = Split.splitOn "," (head allLines)
      fieldNamesStripped = map stripFieldName fieldNames
      fieldTypes = map fieldNameToType fieldNames
      rows = map (Split.splitOn ",") (tail allLines)
      cols = rowsToCols rows
      statsPerCol = zipWith3 getStatsFromCols fieldNamesStripped fieldTypes cols
  in
  List.intercalate "\n" statsPerCol

getStatsFromCols :: String -> FieldType -> [String] -> String
getStatsFromCols fieldNameStripped Number col =
  fieldNameStripped ++ "\n" ++ stats ++ "\n"
  where stats = List.intercalate "\n" [" count:      " ++ show count,
                                       " null count: " ++ show nullCount,
                                       " min:        " ++ show theMin,
                                       " max:        " ++ show theMax,
                                       " average:    " ++ show avg]
        count = length col
        nullCount = length [x | x <- col, x == ""]
        nums = map readFloat [x | x <- col, x /= ""]
        theMin = minimum nums
        theMax = maximum nums
        avg = sum nums / (fromIntegral count - fromIntegral nullCount)
 
getStatsFromCols fieldNameStripped Text col =
  fieldNameStripped ++ "\n" ++ stats ++ "\n"
  where stats = List.intercalate "\n" [" count:                 " ++ show count,
                                       " null count:            " ++ show nullCount,
                                       " count(shortest value): " ++ (show countShortest) ++ " " ++ shortestValue,
                                       " count(longest value):  " ++ (show countLongest) ++ " " ++ longestValue,
                                       " average length:        " ++ show avgLength]
        count = length col
        nullCount = length [x | x <- col, x == ""]
        shortestValue = List.minimumBy (Ord.comparing length) (List.sort col)
        longestValue = List.maximumBy (Ord.comparing length) (List.sort col)
        countShortest = length [x | x <- col, x == shortestValue]
        countLongest = length [x | x <- col, x == longestValue]
        avgLength = (fromIntegral $ sum (map length col)) / (fromIntegral $ length col)

readFloat :: String -> Float
readFloat s =
  case Read.readMaybe s :: Maybe Float of
    Nothing -> error ("Failed to parse supposed float '" ++ s ++ "'\n")
    Just f -> f

-- |The unspace function removes all the spaces from a string.
unspace :: String -> String
unspace s = [c | c <- s, not(Char.isSpace c)]

data FieldType = Text | Number

fieldNameToType name =
  if "(number)" `List.isSubsequenceOf` name
    then Number
    else Text

-- |The stripFieldName removes an assumed initial double quote and
-- everything from the left paren.
stripFieldName fieldName = drop 1 $ takeWhile (/= '(') fieldName

-- |The rowsToCols function converts a list of rows to a list of columns
rowsToCols :: [[a]] -> [[a]]
rowsToCols rows =
  let first = head rows
  in
  if length first == 0
    then []
    else map head rows : rowsToCols (map tail rows)

