import Data.Fin
import Data.List
import Data.SortedSet
import Data.String.Parser
import Debug.Trace
import System.File

newLine : Parser ()
newLine = skip $ char '\n'

parseFile : Parser (List (List Nat))
parseFile = (some $ some (map finToNat digit) <* newLine) <* eos

listWithIxs : List a -> List (Nat, a)
listWithIxs l = zip  (take (length l) [0..]) l

getLoc : List (List Nat) -> (Nat, Nat) -> Maybe Nat
getLoc grid (rix, cix) = getAt rix grid >>= getAt cix

getLoc' : List (List Nat) -> (Nat, Nat) -> Nat
getLoc' grid loc = case getLoc grid loc of
  Just it => it
  Nothing => ?crash

lineHighIxs : List (Nat, Nat) -> List Nat
lineHighIxs row =
  let addBigTree : (Maybe Nat, List Nat) -> (Nat, Nat) -> (Maybe Nat, List Nat)
      addBigTree (Just maxHeight, res) (ix, height) =
        if height > maxHeight
        then (Just height, ix::res)
        else (Just maxHeight, res)
      addBigTree (Nothing, res) (ix, height) = (Just height, ix::res)
  in
    snd $ foldl addBigTree (Nothing, []) row

getHighIxs : Nat -> List Nat -> List (Nat, Nat)
getHighIxs baseIx row =
  let rowWithIxs = listWithIxs row
      normal = lineHighIxs rowWithIxs
      rev = lineHighIxs (reverse rowWithIxs)
  in
      map (\v => (baseIx, v)) (normal ++ rev)

highIxs : List (List Nat) -> SortedSet (Nat, Nat)
highIxs grid =
  let
    rowHighs = foldl (\acc, (ix, row) => acc ++ getHighIxs ix row) [] (listWithIxs grid)
    colHighs = foldl (\acc, (ix, row) => acc ++ getHighIxs ix row) [] (listWithIxs (transpose grid))
    flippedColHighs = map (\(cix, rix) => (rix, cix)) colHighs
  in
   SortedSet.fromList rowHighs `union` SortedSet.fromList flippedColHighs


data Dirn = N | E | S | W

getNextLoc : (Nat, Nat) -> Dirn -> (Nat, Nat) -> Maybe (Nat, Nat)
getNextLoc (Z, _) N _ = Nothing
getNextLoc (S n, e) N _ = Just (n, e)
getNextLoc (_, Z) W _ = Nothing
getNextLoc (s, S w) W _ = Just (s, w)
getNextLoc (s, e) S (nrow, _) = if S s >= nrow then Nothing else Just (S s, e)
getNextLoc (s, e) E (_, ncol) = if S e >= ncol then Nothing else Just (s, S e)

scenicScoreLOS : Nat -> (Nat, Nat) -> Dirn -> Nat -> List (List Nat) -> Nat
scenicScoreLOS targetHeight curloc dirn dist grid =
  let nrows = length grid
      ncols = length $ fromMaybe [] (getAt 0 grid)
  in
      case getNextLoc curloc dirn (nrows, ncols) of
        Just nextLoc =>
          if getLoc' grid nextLoc >= targetHeight
            then dist + 1 -- finished
            else scenicScoreLOS targetHeight nextLoc dirn (dist + 1) grid
        Nothing => dist

scenicScore : (Nat, Nat) -> List (List Nat) -> Nat
scenicScore loc grid =
  let h = getLoc' grid loc
      n = scenicScoreLOS h loc N 0 grid
      s = scenicScoreLOS h loc S 0 grid
      e = scenicScoreLOS h loc E 0 grid
      w = scenicScoreLOS h loc W 0 grid
  in (n * s * e * w)

flatten : List (List a) -> List a
flatten (l::rest) = l ++ flatten rest
flatten [] = []

bestScenicScore : List (List Nat) -> Nat
bestScenicScore grid =
  let
    nrows = length grid
    ncols = length $ fromMaybe [] (getAt 0 grid)
    gridLocs = map (\colix => listWithIxs (iterateN nrows id colix)) (take ncols [0..])
    gridLocs' : List (Nat, Nat)
    gridLocs' = flatten gridLocs
  in
    foldl (\best, loc =>  max (scenicScore loc grid) best) 0 gridLocs'

main : IO ()
main = do
   Right input <- readFile "../data/day8.data"
         | Left err => printLn err
   case parse parseFile input of
     Right (grid, _) => do
       let res = highIxs grid
       printLn (length $ SortedSet.toList res)
       printLn $ bestScenicScore grid
     Left _ => printLn "parse failed"
