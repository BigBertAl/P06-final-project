module Auxiliaries (
  Bit(..), Bitlist(..), FileContent(..),
  binaryToFile, binaryFromFile, fromWord8, toWord8
) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary as B

data Bit = Zero | One
    deriving (Eq, Show)

newtype Bitlist = Bitlist [Bit]

instance B.Binary Bitlist where
    put (Bitlist l) =
        do B.put (length l)
           B.put (bitsToByteString l)
    get =
        do n <- B.get
           bs <- B.get
           let bits = byteStringToBits n bs
           return (Bitlist bits)

data FileContent
  = FileContent
    { fc_codingTable :: [(Char, Bitlist)]
      , fc_content :: Bitlist }

instance B.Binary FileContent where
    put (FileContent table bits) =
        do B.put table
           B.put bits
    get =
        do table <- B.get
           bits <- B.get
           return (FileContent table bits)

byteStringToBits :: Int -> BS.ByteString -> [Bit]
byteStringToBits n bs =
    let allBits = concatMap fromWord8 (BS.unpack bs)
    in take n allBits -- remove padding bits

-- bitsToByteString pads the last Word8 in the resulting bytestring with zeros.
bitsToByteString :: [Bit] -> BS.ByteString
bitsToByteString topBits = BS.pack (toWord8List topBits)
    where
      toWord8List [] = []
      toWord8List bits =
          let (prefix, suffix) = splitAt 8 bits
          in toWord8 prefix : toWord8List suffix

-- to list of bits must have <= 8 elements
toWord8 :: [Bit] -> Word8
toWord8 bits = go 7 bits
    where
      go :: Int -> [Bit] -> Word8
      go _ [] = 0
      go i (Zero:rest) = go (i - 1) rest
      go i (One:rest) = 2^i + go (i - 1) rest

fromWord8 :: Word8 -> [Bit]
fromWord8 word = go 7 word
    where
      go :: Int -> Word8 -> [Bit]
      go i w
          | i < 0 = []
          | otherwise =
              let x = 2^i
                  this = w `div` x
                  rest = w `mod` x
                  bit = if this == 0 then Zero else One
              in bit : go (i - 1) rest

binaryToFile :: B.Binary a => FilePath -> a -> IO ()
binaryToFile path x = do
  let bs = BSL.toStrict (B.encode x)
  BS.writeFile path bs

binaryFromFile :: B.Binary a => FilePath -> IO a
binaryFromFile path = do
  bs <- BS.readFile path
  pure (B.decode (BSL.fromStrict bs))
