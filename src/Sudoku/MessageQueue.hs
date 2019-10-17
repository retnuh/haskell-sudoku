{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}


module Sudoku.MessageQueue where
import           Protolude
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Monoid
import           Data.Traversable
import qualified Data.DList                    as DList
import           Data.DList                     ( DList )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector, (!?) )


class (Ord a,  Monoid (q a)) => MessageQueue (q :: * -> *) a where
    next :: q a -> Maybe (a, q a)
    len :: q a -> Int

instance (Ord a) => MessageQueue [] a where
    next []       = Nothing
    next (x : xs) = Just (x, xs)
    len = Protolude.length

instance (Ord a) => MessageQueue Set a where
    next q = case Set.toAscList q of
        []       -> Nothing
        (x : xs) -> Just (x, Set.fromAscList xs)
    len = Set.size

instance (Ord a) => MessageQueue DList a where
    next q = case DList.toList q of
        []       -> Nothing
        (x : xs) -> Just (x, DList.fromList xs)
    len = Protolude.length . DList.toList

instance (Ord a) => MessageQueue Vector a where
    next q = (, V.tail q) <$> q !? 0
    len = V.length

wrapAsSet :: (Ord a) => [a] -> Set a
wrapAsSet = Set.fromList

wrapAsList :: (Ord a) =>  [a] -> [a]
wrapAsList = identity

wrapAsDList :: (Ord a) => [a] -> DList a
wrapAsDList = DList.fromList

wrapAsVector :: (Ord a) => [a] -> Vector a
wrapAsVector = V.fromList
