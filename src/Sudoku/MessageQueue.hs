{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Sudoku.MessageQueue where
import           Protolude
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Monoid
import           Data.Traversable
import qualified Data.DList                    as DList
import           Data.DList                     ( DList )

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

wrapAsSet :: (Ord a) => [a] -> Set a
wrapAsSet = Set.fromList

wrapAsList :: (Ord a) =>  [a] -> [a]
wrapAsList = identity

wrapAsDList :: (Ord a) => [a] -> DList a
wrapAsDList = DList.fromList
