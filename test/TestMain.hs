{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Maybe (fromJust)
import qualified Matrix
import Matrix.Vector (Vector (..))
import qualified Matrix.Vector as Vector
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type Zero = 'Vector.Zero
type One = 'Vector.Successor Zero
type Two = 'Vector.Successor One
type Three = 'Vector.Successor Two

vectorToList :: Vector.Vector n a -> [a]
vectorToList Vector.Nil = []
vectorToList (x :> xs) = x : vectorToList xs

matrixToLists :: Matrix.Matrix r c a -> [[a]]
matrixToLists (Matrix.Matrix rows) = Prelude.map vectorToList (vectorToList rows)

matrix2x3 :: Matrix.Matrix Two Three Int
matrix2x3 = fromJust (Matrix.fromList @Two @Three [[1, 2, 3], [4, 5, 6]])

matrix2x2 :: Matrix.Matrix Two Two Int
matrix2x2 = fromJust (Matrix.fromList @Two @Two [[1, 2], [3, 4]])

matrix1x3 :: Matrix.Matrix One Three Int
matrix1x3 = fromJust (Matrix.fromList @One @Three [[7, 8, 9]])

transposeTests :: TestTree
transposeTests =
    testGroup
        "transpose"
        [ testCase "2x3 becomes 3x2" $
            let actual = matrixToLists (Matrix.transpose matrix2x3)
                expected = [[1, 4], [2, 5], [3, 6]]
            in actual @?= expected
        , testCase "2x2 stays symmetric" $
            let actual = matrixToLists (Matrix.transpose matrix2x2)
                expected = [[1, 3], [2, 4]]
            in actual @?= expected
        , testCase "1x3 becomes 3x1" $
            let actual = matrixToLists (Matrix.transpose matrix1x3)
                expected = [[7], [8], [9]]
            in actual @?= expected
        , testCase "double transpose is identity" $
            let actual = matrixToLists (Matrix.transpose (Matrix.transpose matrix2x3))
                expected = matrixToLists matrix2x3
            in actual @?= expected
        ]

multiplyTests :: TestTree
multiplyTests =
    testGroup
        "multiply"
        [ testCase "2x3 by 3x2" $
            let actual = matrixToLists (Matrix.multiply matrix2x3 (Matrix.transpose matrix2x3))
                expected = [[14, 32], [32, 77]]
            in actual @?= expected
        , testCase "2x2 by 2x2" $
            let actual = matrixToLists (Matrix.multiply matrix2x2 matrix2x2)
                expected = [[7, 10], [15, 22]]
            in actual @?= expected
        , testCase "2x3 by 3x1" $
            let actual = matrixToLists (Matrix.multiply matrix2x3 (Matrix.transpose matrix1x3))
                expected = [[50], [122]]
            in actual @?= expected
        ]

tests :: TestTree
tests = do
    testGroup "Matrix operations" [transposeTests, multiplyTests]

main :: IO ()
main = defaultMain tests
