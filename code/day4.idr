import Data.String.Parser
import System.File

Span : Type
Span = (Integer, Integer)

newLine : Parser ()
newLine = skip $ char '\n'

parseLine : Parser (Span, Span)
parseLine = do
  ll <- integer
  skip $ char '-'
  lr <- integer
  skip $ char ','
  rl <- integer
  skip $ char '-'
  rr <- integer
  pure ((ll, lr), (rl, rr))

parseFile : Parser (List (Span, Span))
parseFile = (some $ parseLine <* newLine) <* eos

isContained : Span -> Span -> Bool
isContained (ll, lr) (rl, rr) =
  (ll < rl && lr >= rr) || (rl < ll && rr >= lr) || ll == rl

isOverlapping : Span -> Span -> Bool
isOverlapping (ll, lr) (rl, rr) =
  not ((ll < rl && lr < rl) || (rl < ll && rr < ll))

main : IO ()
main = do
   Right input <- readFile "../data/day4.data"
         | Left err => printLn err
   case parse parseFile input of
     Right (input, _) => do
       let count  = foldr (\(l, r), acc => acc + if isContained l r then 1 else 0) 0 input
       printLn count
       let count2  = foldr (\(l, r), acc => acc + if isOverlapping l r then 1 else 0) 0 input
       printLn count2
     Left _ => printLn "parse failed"
