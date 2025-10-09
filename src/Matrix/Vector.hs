{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, InstanceSigs, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Matrix.Vector
    ( NaturalNumber(..)
    , NaturalNumberSingleton(..)
    , KnownNaturalNumber
    , naturalNumberSingleton
    , naturalNumberSingletonToIntegral
    , Vector(..)
    , map
    , mapMonad
    , head
    , tail
    , index
    , zipWith
    , foldRight
    , dotProduct
    , fromList
    , fromListKnown
    ) where
import Prelude hiding (zipWith, map, head, tail)

data NaturalNumber = Zero | Successor NaturalNumber

data NaturalNumberSingleton (n::NaturalNumber) where
    ZeroSingleton::NaturalNumberSingleton 'Zero
    SuccessorSingleton::NaturalNumberSingleton n -> NaturalNumberSingleton ('Successor n)

class KnownNaturalNumber (n::NaturalNumber) where
    naturalNumberSingleton::NaturalNumberSingleton n

instance KnownNaturalNumber 'Zero where
    naturalNumberSingleton = ZeroSingleton

instance KnownNaturalNumber n => KnownNaturalNumber ('Successor n) where
    naturalNumberSingleton = SuccessorSingleton naturalNumberSingleton

naturalNumberSingletonToIntegral::NaturalNumberSingleton n -> Integer
naturalNumberSingletonToIntegral ZeroSingleton = 0
naturalNumberSingletonToIntegral (SuccessorSingleton n) = 1 + naturalNumberSingletonToIntegral n

data Vector (n::NaturalNumber) a' where
    Nil::Vector 'Zero a'
    (:>)::a' -> Vector n a' -> Vector ('Successor n) a'
infixr 5 :>

head::Vector ('Successor n) a' -> a'
head (x :> _) = x
tail::Vector ('Successor n) a' -> Vector n a'
tail (_ :> xs) = xs
index::NaturalNumberSingleton n -> Vector n a' -> a'
index ZeroSingleton (x :> _) = x
index (SuccessorSingleton n) (_ :> xs) = index n xs
-- (!)::Vector ('Successor n) a' -> Integer -> a'
-- v ! i | i < 0 = error "Negative index"
-- v ! 0 = head v
-- v ! i = tail v ! (i - 1)

deriving instance Functor (Vector n)
deriving instance Foldable (Vector n)
deriving instance Traversable (Vector n)

map::(a' -> b') -> Vector n a' -> Vector n b'
map _ Nil = Nil
map f (x :> xs) = f x :> map f xs

mapMonad::Monad m => (a' -> m b') -> Vector n a' -> m (Vector n b')
mapMonad _ Nil = return Nil
mapMonad f (x :> xs) = do
    y <- f x
    ys <- mapMonad f xs
    return (y :> ys)

zipWith::(a' -> b' -> c') -> Vector n a' -> Vector n b' -> Vector n c'
zipWith _ Nil Nil = Nil
zipWith f (x :> xs) (y :> ys) = f x y :> zipWith f xs ys

foldRight::(a' -> b' -> b') -> b' -> Vector n a' -> b'
foldRight _ z Nil = z
foldRight f z (x :> xs) = f x (foldRight f z xs)

dotProduct::Num a' => Vector n a' -> Vector n a' -> a'
dotProduct v1 v2 = foldRight (+) 0 (Matrix.Vector.zipWith (*) v1 v2)


fromList::NaturalNumberSingleton n -> [a'] -> Maybe (Vector n a')
fromList ZeroSingleton [] = Just Nil
fromList ZeroSingleton _ = Nothing
fromList (SuccessorSingleton _) [] = Nothing
fromList (SuccessorSingleton n) (x:xs) = fmap (x :>) (fromList n xs)

fromListKnown::forall n a'. KnownNaturalNumber n => [a'] -> Maybe (Vector n a')
fromListKnown = fromList (naturalNumberSingleton @n)
