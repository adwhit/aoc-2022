import Data.Fin
import Data.List
import Data.SortedSet
import Data.String.Parser
import Debug.Trace
import System.File

newLine : Parser ()
newLine = skip $ char '\n'

data Inst = AddX Integer | NoOp

parseLine : Parser Inst
parseLine = (string "noop" $> NoOp) <|> (map AddX (token "addx" *> integer ))

parseFile : Parser (List Inst)
parseFile = (some $ parseLine <* newLine) <* eos

processScore : Integer -> Integer -> Integer -> Integer
processScore register cycleCt score = if cycleCt `mod` 40 == 20 then score + register * cycleCt else score

processInst : (Integer, Integer, Integer) -> Inst -> (Integer, Integer, Integer)
processInst (register, cycleCt, score) NoOp =
  let newCycleCt = cycleCt + 1
      newScore = processScore register newCycleCt score
  in
    (register, newCycleCt, newScore)
processInst (register, cycleCt, score) (AddX incr) =
  let newScore = processScore register (cycleCt + 1) score
      newCycleCt = cycleCt + 2
      newScore2 = processScore register newCycleCt newScore
      newRegister = register + incr
  in
    (newRegister, newCycleCt, newScore2)

part1 : List Inst -> Integer
part1 insts = snd . snd $ foldl processInst (1, 0, 0) insts

data Pixel = Lit | Dark

pixelChar : Pixel -> Char
pixelChar Lit = '#'
pixelChar Dark = '.'

processPixel : Integer -> List Pixel -> List Pixel
processPixel register pixels =
  let cycle = cast (length pixels) `mod` 40 in
  if register - 1 == cycle || register == cycle || register + 1 == cycle
    then Lit::pixels
    else Dark::pixels

processInst2 : (Integer, List Pixel) -> Inst -> (Integer, List Pixel)
processInst2 (register, pixels) NoOp = (register, processPixel register pixels)
processInst2 (register, pixels) (AddX incr) =
  let newPixels = processPixel register pixels
      newPixels2 = processPixel register newPixels
      newRegister = register + incr
  in
    (newRegister, newPixels2)

part2 : List Inst -> String
part2 insts =
  let chars = map pixelChar $  snd  $ foldl processInst2 (1, []) insts
      splitChars = unfoldr (\l => if length l >= 40 then Just (splitAt 40 l) else Nothing) chars
  in
    pack $ reverse $ intercalate ['\n'] splitChars

main : IO ()
main = do
   Right input <- readFile "../data/day10.data"
         | Left err => printLn err
   case parse parseFile input of
     Right (input, _) => do
       printLn $ part1 input
       putStrLn $ part2 input
     Left _ => printLn "parse failed"
