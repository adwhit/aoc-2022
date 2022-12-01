import Data.List
import Data.String.Parser
import System.File
import Debug.Trace

contiguousRows : Parser (List Integer)
contiguousRows =
  some (do v <- integer
           skip $ char '\n'
           pure v)

parseFile : Parser (List (List Integer))
parseFile =
  some (do contig <- contiguousRows
           eos <|> (skip $ char '\n')
           pure contig)

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
   Right input <- readFile "day1.txt"
         | Left err => printLn err
   case parse parseFile input of
     Right rows => do
       printLn $ show $ (findMax . fst) rows
       printLn $ show $ (findMax3 . fst) rows
     Left _ => printLn "parse failed"
