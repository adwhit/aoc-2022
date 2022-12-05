import Data.String.Parser
import Data.List
import Data.List1
import Data.Fin
import System.File
import Debug.Trace

record Input where
  constructor MkInput
  crates : List (List Char)
  moves : List (Integer, Fin 10, Fin 10)

newLine : Parser ()
newLine = skip $ char '\n'

crate : Parser Char
crate = char '[' *> letter <* char ']'

crateOrSpace : Parser (Maybe Char)
crateOrSpace = (map Just crate) <|> string "   " $> Nothing

crates : Parser (List1 (Maybe Char))
crates = sepBy1 crateOrSpace (char ' ')

crateRows : Parser (List (List1 (Maybe Char)))
crateRows = some $ crates <* newLine

moveRow : Parser (Integer, Fin 10, Fin 10)
moveRow = do
  skip $ string "move "
  amount <- integer
  skip $ string " from "
  loc1 <- digit
  skip $ string " to "
  loc2 <- digit
  pure (amount, loc1, loc2)

mkCrateStack : List (List1 (Maybe Char)) -> List (List Char)
mkCrateStack stack =
  let llmb = transpose $ map toList stack
    in
  map catMaybes llmb

parseFile : Parser Input
parseFile = do
  theCrates <- crateRows
  (sepBy1 (space *> digit *> space) space) *> newLine
  newLine
  moves <- some $ moveRow <* newLine
  eos
  pure (MkInput (mkCrateStack theCrates) moves)

Place = List Char -> List Char -> Integer -> (List Char, List Char)

placeN1 : List Char -> List Char -> Integer -> (List Char, List Char)
placeN1 from to 0 = (from, to)
placeN1 (take::rest) to qty = placeN1 rest (take::to) (qty - 1)
placeN1 [] to qty = ?unreachable1

placeN2 : List Char -> List Char -> Integer -> List Char -> (List Char, List Char)
placeN2 from to 0 acc = (from, (reverse acc) ++ to)
placeN2 (take::rest) to qty acc = placeN2 rest to (qty - 1) (take::acc)
placeN2 [] to qty acc = ?unreachable

replaceAt : List a -> Nat -> a -> List a
replaceAt (x::rest) (S n) val = x :: replaceAt rest n val
replaceAt (_::rest) Z val = val :: rest
replaceAt [] _ _ = []

unwrap : Maybe a -> a
unwrap (Just v) = v
unwrap Nothing = ?hole

applyMove : Place -> List (List Char) -> (Integer, Fin 10, Fin 10) -> List (List Char)
applyMove placeN stacks (qty, from, to) =
  let fromIx = cast (from - 1)
      toIx = cast (to - 1)
      fromStack = unwrap $ getAt fromIx stacks
      toStack = unwrap $ getAt toIx stacks
      (newFrom, newTo) = placeN fromStack toStack qty
      newStack1 = replaceAt stacks fromIx newFrom
      newStack2 = replaceAt newStack1 toIx newTo
  in
    newStack2

getTopOfStack : List (List Char) -> List Char
getTopOfStack input = map (\l => fromMaybe '.' (head' l)) input

countCrates : List (List a) -> Nat
countCrates c = sum $ map length c

showStack : List (List Char) -> List (List Char)
showStack stack =
  let packed = traceVal $ map pack stack
  in map unpack packed

main : IO ()
main = do
   Right input <- readFile "../data/day5.data"
         | Left err => printLn err
   case parse parseFile input of
     Right (input, _) => do
       let out1 = foldl (\stack, move => applyMove placeN1 stack move) input.crates input.moves
       printLn $ pack $ getTopOfStack out1
       let placeN2' = \from, to, qty => placeN2 from to qty []
       let out2 = foldl (\stack, move => applyMove placeN2' stack move) input.crates input.moves
       printLn $ pack $ getTopOfStack out2
     Left _ => printLn "parse failed"
