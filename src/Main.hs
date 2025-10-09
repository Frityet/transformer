module Main where

import qualified Data.Maybe

import Tokeniser (encode, decode)
import Data.Function ((&))

main :: IO ()
main =
    let msg = encode "hélló!"
        in do
        print msg
        print (decode msg)
