{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, InstanceSigs, TypeOperators, ScopedTypeVariables #-}

module Matrix (Matrix(..), fromList, transpose, multiply) where

import qualified Matrix.Vector as Vector
import Matrix.Vector(Vector((:>)))
import Data.Function ((&))

data Matrix (r::Vector.NaturalNumber) (c::Vector.NaturalNumber) a' where
    Matrix::Vector.Vector r (Vector.Vector c a') -> Matrix r c a'

fromList::forall r c a'. (Vector.KnownNaturalNumber r, Vector.KnownNaturalNumber c) => [[a']] -> Maybe (Matrix r c a')
fromList rows = Matrix <$> (Vector.fromListKnown @r =<< traverse (Vector.fromListKnown @c) rows)

transpose::forall r c a'. (Vector.KnownNaturalNumber r, Vector.KnownNaturalNumber c) => Matrix r c a' -> Matrix c r a'
transpose (Matrix rows) = Matrix (go (Vector.naturalNumberSingleton @c) rows) where
    go::Vector.NaturalNumberSingleton n -> Vector.Vector r (Vector.Vector n a') -> Vector.Vector n (Vector.Vector r a')
    go Vector.ZeroSingleton _ = Vector.Nil
    go (Vector.SuccessorSingleton n) vs =
        let column = Vector.map Vector.head vs
            remainder = Vector.map Vector.tail vs
        in column :> go n remainder

multiply::forall r c d a'. (Vector.KnownNaturalNumber r, Vector.KnownNaturalNumber c, Vector.KnownNaturalNumber d, Num a') => Matrix r c a' -> Matrix c d a' -> Matrix r d a'
multiply a b =
    let Matrix rowsA = a
        Matrix rowsB = transpose b
        in Matrix (rowsA & Vector.map (\rowA -> rowsB & Vector.map (Vector.dotProduct rowA)))
