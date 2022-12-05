import Data.List
import Data.String.Parser
import System.File
import Debug.Trace

data Move = Rock | Paper | Scissors
data Outcome = Win | Lose | Draw

newLine : Parser ()
newLine = skip $ char '\n'

getMoveL : Parser Move
getMoveL = (char 'A' $> Rock)
           <|> (char 'B' $> Paper)
           <|> (char 'C' $> Scissors)

getMoveR : Parser Move
getMoveR  = (char 'X' $> Rock)
           <|> (char 'Y' $> Paper)
           <|> (char 'Z' $> Scissors)

getMoveR2 : Parser Outcome
getMoveR2  = (char 'X' $> Lose)
            <|> (char 'Y' $> Draw)
            <|> (char 'Z' $> Win)

parseFile : Parser a -> Parser (List (Move, a))
parseFile getMove = many line3 <* eos
  where
    line3 : Parser (Move, a)
    line3 = do
      l <- getMoveL
      spaces
      r <- getMove
      newLine
      pure (l, r)

scoreWin : Move -> Move -> Integer
scoreWin Rock Scissors = 0
scoreWin Rock Rock = 3
scoreWin Rock Paper = 6
scoreWin Scissors Scissors = 3
scoreWin Scissors Rock = 6
scoreWin Scissors Paper = 0
scoreWin Paper Scissors = 6
scoreWin Paper Rock = 0
scoreWin Paper Paper = 3

getIndicatedMove : Move -> Outcome -> Move
getIndicatedMove m Draw = m
getIndicatedMove Rock Win = Paper
getIndicatedMove Rock Lose = Scissors
getIndicatedMove Paper Win = Scissors
getIndicatedMove Paper Lose = Rock
getIndicatedMove Scissors Win = Rock
getIndicatedMove Scissors Lose = Paper

scoreMove : Move -> Integer
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreRound : Move -> Move -> Integer
scoreRound l r = scoreWin l r + scoreMove r

scoreRound2 : Move -> Outcome -> Integer
scoreRound2 l r = scoreRound l $ getIndicatedMove l r

main : IO ()
main = do
   Right input <- readFile "../data/day2.data"
         | Left err => printLn err
   case parse (parseFile getMoveR) input of
     Right rows => do
       let score: List Integer = map (\(l, r) => scoreRound l r) $ fst rows
       printLn $ sum score
     Left _ => printLn "parse failed"
   case parse (parseFile getMoveR2) input of
     Right rows => do
       let score: List Integer = map (\(l, r) => scoreRound2 l r) $ fst rows
       printLn $ sum score
     Left _ => printLn "parse failed"
