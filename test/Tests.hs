module Main where

import Huffman
import Test.HUnit
import qualified Data.Map.Strict as Map
import Auxiliaries

test_isConsistent :: IO ()
test_isConsistent = do
    let validTree = isConsistent exampleTree
    let invalidTree = isConsistent (Node (Leaf 'E' 158) (Leaf 'N' 95) 250)
    assertEqual "Valid tree is consistent" True validTree
    assertEqual "Invalid tree is not consistent" False invalidTree

test_toCodingTable :: IO ()
test_toCodingTable = do
  let codingTable = exampleCodingTable
  assertEqual "E" (Just [Zero, Zero]) (Map.lookup 'E' codingTable)
  assertEqual "A" (Just [Zero, One, One, One]) (Map.lookup 'A' codingTable)
  assertEqual "R" (Just [One, One, Zero]) (Map.lookup 'R' codingTable)


test_encode :: IO ()
test_encode = do
  assertEqual "encoding EAR" [Zero, Zero, Zero, One, One, One, One, One, Zero] (encode exampleCodingTable "EAR")

test_decode :: IO ()
test_decode = do
  assertEqual "decoding 000111110" "EAR" (decode exampleTree [Zero, Zero, Zero, One, One, One, One, One, Zero] )

test_inverseOfEncodeAndDecode :: IO ()
test_inverseOfEncodeAndDecode = do
  assertEqual "Inverse with EAR" "EAR" (decode exampleTree (encode exampleCodingTable "EAR"))
  assertEqual "Inverse with 000111110" [Zero, Zero, Zero, One, One, One, One, One, Zero] (encode exampleCodingTable (decode exampleTree [Zero, Zero, Zero, One, One, One, One, One, Zero]))

test_buildHTree :: IO ()
test_buildHTree = do
  let createdTree = isConsistent (buildHTree "HelloWorld") 
  assertEqual "Building Tree from String HelloWorld" True createdTree

test_toDecodeTree :: IO ()
test_toDecodeTree = pure ()

test_toFromWord8 :: IO ()
test_toFromWord8 = pure ()

test_encodeDecodeFile :: IO ()
test_encodeDecodeFile = do
    let inputPath = "input.txt"
        encodedPath = "output.comp"
        decodedPath = "input2.txt"
    encodeFile inputPath encodedPath
    decodeFile encodedPath decodedPath
    originalContent <- readFile inputPath
    decodedContent <- readFile decodedPath
    assertEqual "Encoded then decoded content should match original" originalContent decodedContent
  
allTests :: Test
allTests =
  TestList
    [ mkTest "toCodingTable" test_toCodingTable
    , mkTest "encode" test_encode
    , mkTest "isConsistent" test_isConsistent
    , mkTest "decode" test_decode
    , mkTest "inverse" test_inverseOfEncodeAndDecode
    , mkTest "buildHTree" test_buildHTree
    , mkTest "toDecodeTree" test_toDecodeTree
    , mkTest "toFromWord8" test_toFromWord8
    , mkTest "encodeDecodeFile" test_encodeDecodeFile
    ]
  where
    mkTest label ass = TestLabel label (TestCase ass)

main :: IO ()
main = runTestTTAndExit allTests
