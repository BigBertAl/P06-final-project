module Huffman where
import qualified Data.Map.Strict as Map
import Data.Maybe 
import Auxiliaries
import Data.List
import qualified Data.Map as M
import Data.Function


type Map = Map.Map
type CodingTable = Map Char [Bit]

data HTree = Leaf Char Int | Node HTree HTree Int
    deriving Show

getNumber :: HTree -> Int
getNumber (Leaf _ num) = num
getNumber (Node _ _ num) = num

merge :: HTree -> HTree -> HTree
merge tree1 tree2 = Node tree1 tree2 (getNumber tree1 + getNumber tree2) 

exampleTree :: HTree
exampleTree = Node 
    (Node                       -- left
        (Leaf 'E' 158)          -- left
        (Node                   -- right
            (Leaf 'S' 67)       -- left
            (Node               -- right
                (Leaf 'T' 64)   -- left
                (Leaf 'A' 61)   -- right
                125
            )
            192
        ) 
        350
    )
    (Node                       -- right
        (Leaf 'N' 97)           -- left
        (Node                   -- right
            (Leaf 'R' 77)       -- left
            (Leaf 'I' 82)       -- right
            159
        ) 
        256
    )               
    606

emptyTree :: HTree
emptyTree = Node (Leaf '\0' 0) (Leaf '\0' 0) 0

isConsistent :: HTree -> Bool
isConsistent (Leaf _ _) = True
isConsistent (Node left right freq) =
    freq == (getFreq left + getFreq right) && isConsistent left && isConsistent right
  where
    getFreq (Leaf _ f) = f
    getFreq (Node _ _ f) = f

exampleCodingTable :: CodingTable
exampleCodingTable = toCodingTable exampleTree

toCodingTable :: HTree -> CodingTable
toCodingTable tree = Map.fromList $ getBits tree []

getBits :: HTree -> [Bit] -> [(Char, [Bit])]
getBits (Leaf char _) bits = [(char, bits)]
getBits (Node left right _) bits =
    getBits left (bits ++ [Zero]) ++ getBits right (bits ++ [One])

encode :: CodingTable -> String -> [Bit]
encode _ [] = []
encode table (char:rest) = fromMaybe [] (Map.lookup char table) ++ encode table rest

decode :: HTree -> [Bit] -> String
decode tree bits = getChars tree bits
    where
        getChars (Leaf char _) restBits = char : getChars tree restBits
        getChars (Node left _ _) (Zero:restBits) = getChars left restBits
        getChars (Node _ right _) (One:restBits) = getChars right restBits
        getChars _ _ = []

getFrequencies :: String -> [(Char, Int)]
getFrequencies = M.toList . M.fromListWith (+) . map (flip (,) 1)

buildHTree :: String -> HTree
buildHTree text = buildTree (getFrequencies text)

buildTree :: [(Char, Int)] -> HTree
buildTree = build . map (uncurry Leaf) . sortBy (compare `on` snd)
    where   build (leaf:[])    = leaf
            build (leaf1:leaf2:rest)  = build $ insertBy (compare `on` getNumber) (merge leaf1 leaf2) rest
            build [] = error "buildTree: empty list"

toDecodeTree :: CodingTable -> HTree
toDecodeTree codingTable = foldl' insertInTree emptyTree (Map.toList codingTable)

insertInTree :: HTree -> (Char, [Bit]) -> HTree
insertInTree tree (char, code) = insertChar tree char code

insertChar :: HTree -> Char -> [Bit] -> HTree    
insertChar _ char [] = Leaf char 0
insertChar (Node left right _) char (Zero:cs) = Node (insertChar left char cs) right 0
insertChar (Node left right _) char (One:cs) = Node left (insertChar right char cs) 0
insertChar (Leaf _ _) char (Zero:cs) = Node (insertChar (Leaf '0' 0) char cs) (Leaf '0' 0) 0
insertChar (Leaf _ _) char (One:cs) = Node (Leaf '0' 0) (insertChar (Leaf '0' 0) char cs) 0

toFileContent :: CodingTable -> [Bit] -> FileContent
toFileContent codingTable bits = FileContent (map (\(char, bits) -> (char, Bitlist bits)) (Map.toList codingTable)) (Bitlist bits)

fromFileContent :: FileContent -> (CodingTable, [Bit])
fromFileContent (FileContent table (Bitlist bits)) = ( Map.fromList(map(\(char, (Bitlist bits)) -> (char, bits)) table), bits)

encodeFile :: FilePath -> FilePath -> IO ()
encodeFile inputPath outputPath = do
    inputText <- readFile inputPath
    let huffmanTree = buildHTree inputText
        codingTable = toCodingTable huffmanTree
        encodedBits = encode codingTable inputText
        fileContent = toFileContent codingTable encodedBits
    binaryToFile outputPath fileContent

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile inputPath outputPath = do
    fileContent <- binaryFromFile inputPath
    let (codingTable, encodedBits) = fromFileContent fileContent
        huffmanTree = toDecodeTree codingTable
        outputText = decode huffmanTree encodedBits 
    writeFile outputPath outputText