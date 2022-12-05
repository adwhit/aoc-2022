import Data.List
import Data.String.Parser
import System.File
import Debug.Trace

contiguousRows : Parser (List Integer)
contiguousRows =
  some (integer <* char '\n')

parseFile : Parser (List (List Integer))
parseFile =
  some (contiguousRows <* (eos <|> (skip $ char '\n')))

findMax : List (List Integer) -> Integer
findMax rows = foldl pickAccOrSum 0 rows
  where
    pickAccOrSum : Integer -> List Integer -> Integer
    pickAccOrSum acc rows' = max acc (sum rows')

findMax3 : List (List Integer) -> Integer
findMax3 rows = sum top3
  where
    totals : List Integer
    totals = reverse $ sort $ map sum rows
    top3 : List Integer
    top3 = take 3 totals

main : IO ()
main = do
   Right input <- readFile "../data/day1.data"
         | Left err => printLn err
   case parse parseFile input of
     Right rows => do
       printLn $ show $ (findMax . fst) rows
       printLn $ show $ (findMax3 . fst) rows
     Left _ => printLn "parse failed"
