module Tokeniser (encode, decode) where

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Ord as Ordering
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Word (Word8)
import Data.Function ((&))
import Control.Category ((>>>))

type Token = Int
type Piece = ByteString.ByteString

idToToken :: IntMap.IntMap Piece
idToToken = IntMap.fromList [ (i, ByteString.singleton (fromIntegral i :: Word8)) | i <- [0..255] ]

tokenToId :: Map.Map Piece Token
tokenToId = Map.fromList [ (v, k) | (k, v) <- IntMap.toList idToToken ]

toBytes :: String -> [Word8]
toBytes = Text.pack >>> Encoding.encodeUtf8 >>> ByteString.unpack

encode :: String -> [Token]
encode = toBytes >>> map (ByteString.singleton >>> (tokenToId Map.!))

decode :: [Token] -> Maybe String
decode = mapM (`IntMap.lookup` idToToken) >>> fmap (ByteString.concat >>> Encoding.decodeUtf8 >>> Text.unpack)

type Pair = (Piece, Piece)


pairFrequencies :: [Piece] -> Map.Map Pair Int
pairFrequencies [] = Map.empty
pairFrequencies [_] = Map.empty
pairFrequencies pieces = Map.fromListWith (+) [ ((a, b), 1) | (a, b) <- pieces `zip` tail pieces ]

mostFrequentPair :: [Piece] -> Maybe Pair
mostFrequentPair [] = Nothing
mostFrequentPair x = x & pairFrequencies & Map.toList & List.maximumBy (Ordering.comparing snd) & fst & Just

replacePair :: Pair -> Piece -> [Piece] -> [Piece]
replacePair _ _ [] = []
replacePair _ _ [x] = [x]
replacePair pair newPiece (x:y:xs)
    | (x, y) == pair = newPiece : replacePair pair newPiece xs
    | otherwise = x : replacePair pair newPiece (y:xs)


bytePairEncoding :: [Piece] -> [Piece]
bytePairEncoding [] = []
bytePairEncoding [x] = [x]
bytePairEncoding pieces =
    let mostFrequent = mostFrequentPair pieces
        in case mostFrequent of
            Nothing -> []
            Just pair -> replacePair pair (uncurry ByteString.append pair) pieces

learnMerges :: Int -> [Piece] -> ([Pair], [Piece])
learnMerges 0 m = ([], m)
learnMerges _ [] = ([], [])
learnMerges k m =
    let best = mostFrequentPair m
        in case best of
            Nothing -> ([], m)
            Just pair ->
                let newPiece = uncurry ByteString.append pair
                    newM = replacePair pair newPiece m
                    (restPairs, finalM) = learnMerges (k - 1) newM
                    in (pair : restPairs, finalM)
