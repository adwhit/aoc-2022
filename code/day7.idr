import Data.String.Parser
import Data.List
import Data.SortedMap
import Data.Maybe
import System.File
import Debug.Trace

data FileSys = File Integer String | Dir String

fileSize : FileSys -> Integer
fileSize (File s _) = s
fileSize (Dir _) = 0

data Command =
  Cd String | CdUp | CdRoot | Ls (List FileSys)

newLine : Parser ()
newLine = skip $ char '\n'

word : Parser String
word = pack <$> some letter

parseCd : Parser Command
parseCd = (string ".." $> CdUp)  <|> (string "/" $> CdRoot) <|> (Cd <$> word)

parseLsFile : Parser FileSys
parseLsFile = do
  size <- integer
  skip space
  pre <- word
  post <- optional $ char '.' *> word
  pure (File size $ pre ++ fromMaybe "" post)

parseFs : Parser FileSys
parseFs = (string "dir " *> (Dir <$> word)) <|> parseLsFile

parseLsOutput : Parser (List FileSys)
parseLsOutput = some $ parseFs <* newLine

parseCmd : Parser Command
parseCmd = (string "$ cd " *> parseCd <* newLine)
  <|> (string "$ ls" *> newLine *> Ls <$> parseLsOutput)

parseFile : Parser (List Command)
parseFile = (some $ parseCmd) <* eos

data Tree = Node (List FileSys) (SortedMap String Tree)

newNode : Tree
newNode  = Node [] empty

addNode : Tree -> List String -> List FileSys -> Tree
addNode (Node nfs children) (dir::[]) fs = Node nfs $ insert dir (Node fs empty) children
addNode (Node nfs children) (dir::path) fs =
  let newChild = addNode (fromMaybe newNode $ lookup dir children) path fs
  in Node nfs $ insert dir newChild children
addNode _ [] _ = ?cannot

buildTree : List Command -> Tree -> List String -> Tree
buildTree (CdRoot::rest) tree path = buildTree rest tree path
buildTree (CdUp::rest) tree (_::path) = buildTree rest tree path
buildTree (CdUp::rest) tree [] = ?fail
buildTree ((Cd dir)::rest) tree path = buildTree rest tree (dir::path)
buildTree ((Ls fs)::rest) tree path =
  let newTree = addNode tree (reverse path) fs
   in buildTree rest newTree path
buildTree [] tree path = tree

dirSize : Tree -> Integer
dirSize (Node fs _) = foldr (\elem, acc => acc + fileSize elem) 0 fs

getNodeSizes : Tree -> List Integer -> (Integer, List Integer)
getNodeSizes n@(Node _ children) acc =
  let (tot, acc) = foldr (\node, (tot, res) => let (tot', res') = getNodeSizes node res in (tot + tot', res')) (0, acc) children
      size = tot + dirSize n
  in
  (size, (size::acc))

main : IO ()
main = do
   Right input <- readFile "../data/day7.data"
         | Left err => printLn err
   case parse parseFile input of
     Right (commands, _) => do
       let tree = buildTree commands newNode ["/"]
       let (rootSize, res) = getNodeSizes tree []
       printLn $ sum $ filter (\v => v <= 100000) res
       let unused = 70000000 - rootSize
       let target = 30000000 - unused
       let bestDir = foldr (\elem, best => if elem - target > 0 && elem < best then elem else best) 100000000 res
       printLn bestDir
     Left _ => printLn "parse failed"
