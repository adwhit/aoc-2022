import Data.List
import Data.Maybe
import System.File

window4 : List t -> List (List t)
window4 (a::b::c::d::rest) = [a, b, c, d] :: window4 (b::c::d::rest)
window4 _ = []

windows : Nat -> List t -> List (List t)
windows width l@(a::rest) = if (length l) >= width then (take width l) :: windows width rest else []
windows _ _ = []

unique : Eq a => List a -> Bool
unique (a::b::rest) = if a == b then False else unique (b::rest)
unique _ = True

isUnique : (Ord a, Eq a) => (Nat, List a) -> Maybe Nat
isUnique (ix, l) = if unique (sort l) then Just ix else Nothing

main : IO ()
main = do
  Right input <- readFile "../data/day6.data"
    | Left err => printLn err

  -- Pt 1
  let window' = window4 $ unpack input
  let zipped = zip  (take (length window')[4..]) window'
  let uniqIx = foldl (\acc, pair => if isJust acc then acc else isUnique pair) Nothing zipped
  printLn uniqIx

  -- Pt 2
  let window2 = windows 14 $ unpack input
  let zipped2 = zip  (take (length window2)[14..]) window2
  let uniqIx2 = foldl (\acc, pair => if isJust acc then acc else isUnique pair) Nothing zipped2
  printLn uniqIx2
