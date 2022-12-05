import Data.List
import Data.Nat
import Data.String
import Data.Maybe as M
import System.File
import Data.SortedSet
import Debug.Trace

half : Nat -> Nat
half num = divNatNZ num 2 SIsNonZero


findDuplicate : List String -> Maybe Char
findDuplicate lines =
  let sets = map (\l => fromList $ unpack l) lines
      firstSet = fromMaybe SortedSet.empty $ head' sets
      shared = foldr (\set, common => intersection common set) firstSet sets
      diff = SortedSet.toList shared
    in
  if (length diff) > 1 then Nothing else head' $ diff

part1' : String -> Maybe Char
part1' line =
  let
    splitIx = half $ length line
    leftStr = substr 0 splitIx line
    rightStr = substr splitIx (length line) line
  in findDuplicate [leftStr, rightStr]

scoreChar : Char -> Int
scoreChar c = if isUpper c then
    ord c - ord 'A' + 27
  else
    ord c - ord 'a' + 1

part1 : List String -> Maybe Int
part1 lines =
  let mbDupes: Maybe (List Char) = sequence $ map part1' lines
      addScores : List Char -> Int
      addScores elems = foldr (\elem, acc => acc + scoreChar elem) 0 elems
    in
  map addScores mbDupes

part2' : List String -> Maybe Int -> Maybe Int
part2' (f::s::t::rest) (Just acc) = part2' rest $ map (\c => acc + scoreChar c) $ findDuplicate [f,s,t]
part2' [] v = v
part2' a b = Nothing

part2 : List String -> Maybe Int
part2 lines = part2' lines $ Just 0

main : IO ()
main = do
  Right input <- readFile "../data/day3.data"
    | Left err => printLn err
  case part1 $ lines input of
    Just score => printLn score
    Nothing => printLn "pt1 error"
  case part2 $ lines input of
    Just score => printLn score
    Nothing => printLn "pt1 error"
  pure ()
