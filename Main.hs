#!/usr/bin/env runhaskell

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Text.Read as Read
import qualified Data.Tuple as Tuple

main = do
  input <- Tio.getContents
  let allLines = map unspace $ T.lines input
      lineCount = length allLines
      fieldNames = T.splitOn (T.pack ",") (head allLines)
      fieldNamesStripped = map stripFieldName fieldNames
      fieldTypes = map fieldNameToType fieldNames
      rows = map (T.splitOn (T.pack ",")) (tail allLines)
      statsPerCol = getStatsFromRows fieldNames fieldTypes rows
      statsDisplayed = map display statsPerCol
      statsWithNames = zipWith (\ name stats -> T.unlines [name, stats]) fieldNamesStripped statsDisplayed
  Tio.putStrLn $ T.intercalate (T.pack "\n\n") statsWithNames

display :: Stats -> T.Text
display NumberStats { count = count, nullCount = nullCount, least = least, most = most, total = total } =
  T.pack $
    List.intercalate "\n" [" count:      " ++ show count,
                           " null count: " ++ show nullCount,
                           " min:        " ++ show least,
                           " max:        " ++ show most,
                           " average:    " ++ show avg]
  where avg = total / fromIntegral (count - nullCount)

display TextStats { stringToCount = stringToCount } =
  T.pack $
    List.intercalate "\n" [" count:                 " ++ show count,
                           " null count:            " ++ show nullCount,
                           " count(shortest value): " ++ (show countShortest) ++ " " ++ T.unpack shortestValue,
                           " count(longest value):  " ++ (show countLongest) ++ " " ++ T.unpack longestValue,
                           " average length:        " ++ show avgLength]
  where count = sum (Map.elems stringToCount)
        nullCount = Map.findWithDefault 0 T.empty stringToCount
        countShortest = Map.findWithDefault 0 shortestValue stringToCount
        countLongest = Map.findWithDefault 0 longestValue stringToCount
        -- The sort here and for longestValue is to break ties alphabetically.
        shortestValue = head $ List.sort $ filter strIsOneOfTheShortest allStrings
        strIsOneOfTheShortest = \ str -> (not $ isNull str) && T.length str == shortestLength
        shortestLength = T.length $ head strsByLength
        longestValue = head $ List.sort $ filter strIsOneOfTheLongest allStrings
        strIsOneOfTheLongest = \ str -> T.length str == longestLength
        longestLength = T.length $ head $ reverse strsByLength
        strsByLength = List.sortOn T.length $ filter (not . isNull) $ Map.keys stringToCount
        avgLength = fromIntegral lengthTotal / (fromIntegral count - fromIntegral nullCount)
        lengthTotal = sum $ map (\(s, count) -> count * T.length s) stringToCountAsList
        allStrings = map Tuple.fst stringToCountAsList
        stringToCountAsList = Map.toList stringToCount

getStatsFromRows :: [T.Text] -> [FieldType] -> [[T.Text]] -> [Stats]
getStatsFromRows fieldNames fieldTypes rows =
  getStatsFromRows2 fieldNames fieldTypes rows initialStats
  where initialStats = map makeInitialStats fieldTypes

makeInitialStats :: FieldType -> Stats
makeInitialStats Number = NumberStats { count = 0, nullCount = 0, least = infinity, most = -infinity, total = 0 }
makeInitialStats Text = TextStats { stringToCount = Map.empty }

infinity :: Double
infinity = read "Infinity"

data Stats =
    NumberStats { count :: Int, nullCount :: Int, least :: Double, most :: Double, total :: Double }
  | TextStats { stringToCount :: Map.Map T.Text Int }
  deriving Show

getStatsFromRows2 :: [T.Text] -> [FieldType] -> [[T.Text]] -> [Stats] -> [Stats]
getStatsFromRows2 _ _ [] statsAcc = statsAcc
getStatsFromRows2 fieldNames fieldTypes (row:rows) statsAcc =
  let statsAcc2 = zipWith3 updateStats fieldTypes row statsAcc in
  getStatsFromRows2 fieldNames fieldTypes rows statsAcc2

updateStats :: FieldType -> T.Text -> Stats -> Stats
updateStats Number cell (stats @ NumberStats { count = count, nullCount = nullCount, least = least, most = most, total = total }) =
  if isNull cell
    then stats { nullCount = nullCount + 1 }
    else
      case Read.double cell of
        Left _ -> error ("Failed to read supposed number " ++ T.unpack cell)
        Right (num, _) ->
            NumberStats { count = count + 1,
                          nullCount = nullCount,
                          least = min least num,
                          most = max most num,
                          total = total + num }

updateStats Text cell (TextStats { stringToCount = stringToCount }) =
  TextStats { stringToCount = Map.insertWith (+) cell 1 stringToCount }

isNull :: T.Text -> Bool
isNull = T.null

-- |The unspace function removes all the spaces from a string.
unspace :: T.Text -> T.Text
unspace s = T.filter (not . Char.isSpace) s

data FieldType = Text | Number

fieldNameToType :: T.Text -> FieldType
fieldNameToType name =
  if T.pack "(number)" `T.isInfixOf` name
    then Number
    else Text

-- |The stripFieldName function removes an assumed initial double quote and
-- everything from the left paren.
stripFieldName fieldName = T.drop 1 $ T.takeWhile (/= '(') fieldName

