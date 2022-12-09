import System.File
import Data.String.Parser
import Data.SortedSet
import Data.List
import Data.List1
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

moveHead : Posn -> Dirn -> Posn
moveHead (x, y) U = (x, y + 1)
moveHead (x, y) D = (x, y - 1)
moveHead (x, y) R = (x + 1, y)
moveHead (x, y) L = (x - 1, y)

moveLookup : SortedMap Posn Posn
moveLookup = SortedMap.fromList [
  ((2, 0), (1, 0)),
  ((-2, 0), (-1, 0)),
  ((0, 2), (0, 1)),
  ((0, -2), (0, -1)),
  ((1, 2), (1, 1)),
  ((2, 1), (1, 1)),
  ((2, 2), (1, 1)),
  ((-1, 2), (-1, 1)),
  ((-2, 1), (-1, 1)),
  ((-2, 2), (-1, 1)),
  ((-1, -2), (-1, -1)),
  ((-2, -1), (-1, -1)),
  ((-2, -2), (-1, -1)),
  ((1, -2), (1, -1)),
  ((2, -1), (1, -1)),
  ((2, -2), (1, -1))
  ]

moveTail : Posn -> Posn -> Posn
moveTail (hx, hy) (tx, ty) =
  let (xdiff, ydiff) = fromMaybe (0, 0) $ lookup (hx - tx, hy - ty) moveLookup
  in
  (tx + xdiff, ty + ydiff)

applyTailMoves : Posn -> List Posn -> List Posn
applyTailMoves head (t::rest) =
  let newT = moveTail head t
  in newT::(applyTailMoves newT rest)
applyTailMoves head [] = []

applyMoveOnce : List1 Posn -> SortedSet Posn -> Dirn -> (List1 Posn, SortedSet Posn)
applyMoveOnce (head:::rest) set dirn =
  let newHead = moveHead head dirn
      chain = newHead:::(applyTailMoves newHead rest)
      newSet = SortedSet.insert (last chain) set
  in
    (chain, newSet)

applyNTimes : Integer -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

applyMove : List1 Posn -> SortedSet Posn ->  Move -> (List1 Posn, SortedSet Posn)
applyMove posns set (dirn, rpt) =
  applyNTimes rpt (\(posns', set') => applyMoveOnce posns' set' dirn) (posns, set)

part1 : List Move -> Nat
part1 moves =
  let startposns = (0, 0):::[(0, 0)]
      (endposns, uniqt) = foldl (\(posns, uniqtpos), move => applyMove posns uniqtpos move) (startposns, empty) moves
 in
   length $ SortedSet.toList uniqt

part2 : List Move -> Nat
part2 moves =
  let startposns = (0, 0):::(iterateN 9 id (0, 0))
      (endposns, uniqt) = foldl (\(posns, uniqtpos), move => applyMove posns uniqtpos move) (startposns, empty) moves
 in
   length $ SortedSet.toList uniqt

main : IO ()
main = do
   Right input <- readFile "../data/day9.data"
         | Left err => printLn err
   case parse parseInput input of
     Right (input, _) => do
        printLn $ part1 input
        printLn $ part2 input
     Left _ => printLn "parse failed"
