import System.File
import Data.String.Parser
import Data.SortedSet
import Data.Maybe
import Data.SortedMap
import Debug.Trace

data Dirn = U | D | L | R

Posn : Type
Posn = (Integer, Integer)

Move : Type
Move = (Dirn, Integer)

parseLine : Parser Move
parseLine = do
  dirn <- (char 'U' $> U) <|> (char 'D' $> D) <|> (char 'R' $> R) <|> (char 'L' $> L)
  skip $ space
  dist <- integer
  pure (dirn, dist)

parseInput : Parser (List Move)
parseInput = some (parseLine <* char '\n')

movePos : Posn -> Dirn -> Posn
movePos (x, y) U = (x, y + 1)
movePos (x, y) D = (x, y - 1)
movePos (x, y) R = (x + 1, y)
movePos (x, y) L = (x - 1, y)

moveLookup : SortedMap Posn Posn
moveLookup = SortedMap.fromList [
  ((2, 0), (1, 0)),
  ((-2, 0), (-1, 0)),
  ((0, 2), (0, 1)),
  ((0, -2), (0, -1)),
  ((1, 2), (1, 1)),
  ((2, 1), (1, 1)),
  ((-1, 2), (-1, 1)),
  ((-2, 1), (-1, 1)),
  ((-1, -2), (-1, -1)),
  ((-2, -1), (-1, -1)),
  ((1, -2), (1, -1)),
  ((2, -1), (1, -1))
  ]

moveTail : Posn -> Posn -> Posn
moveTail (hx, hy) (tx, ty) =
  let (xdiff, ydiff) = fromMaybe (0, 0) $ lookup (hx - tx, hy - ty) moveLookup
  in
  (tx + xdiff, ty + ydiff)

applyNTimes : Integer -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

applyMoveOnce : Posn -> Posn -> SortedSet Posn -> Dirn -> (Posn, Posn, SortedSet Posn)
applyMoveOnce hpos tpos set dirn =
  let newh = movePos hpos dirn
      newt = moveTail newh tpos
      newSet = insert newt set
  in
    (newh, newt, newSet)

applyMove : Posn -> Posn -> SortedSet Posn ->  Move -> (Posn, Posn, SortedSet Posn)
applyMove hpos tpos set (dirn, rpt) =
  applyNTimes rpt (\(a, b, c) => applyMoveOnce a b c dirn) (hpos, tpos, set)

part1 : List Move -> Nat
part1 moves =
  let hpos = (0, 0)
      tpos = (0, 0)
      (endh, endt, uniqt) = foldl (\(hpos, tpos, uniqtpos), move => applyMove hpos tpos uniqtpos move) (hpos, tpos, empty) moves
 in
   length $ SortedSet.toList uniqt

main : IO ()
main = do
   Right input <- readFile "../data/day9.data"
         | Left err => printLn err
   case parse parseInput input of
     Right (input, _) => do
        printLn $ part1 input
     Left _ => printLn "parse failed"
