{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}


module Sudoku.MessageQueue where
import           Protolude
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Monoid
import           Data.Traversable
import qualified Data.DList                    as DList
import           Data.DList                     ( DList )

class (forall a.  Monoid (q a)) => MessageQueue (q :: * -> *) a where
    next :: (q a) -> Maybe (a, q a)
    length :: (q a) -> Int

instance (forall a. Ord a) => MessageQueue [] a where
    next []       = Nothing
    next (x : xs) = Just (x, xs)
    length = Protolude.length

instance (forall a. Ord a) => MessageQueue Set a where
-- instance MessageQueue Set a where
    next q = case Set.toAscList q of
        []       -> Nothing
        (x : xs) -> Just (x, Set.fromAscList xs)
    length = Set.size

instance (forall a. Ord a) => MessageQueue DList a where
    next q = case DList.toList q of
        []       -> Nothing
        (x : xs) -> Just (x, DList.fromList xs)
    length = Protolude.length . DList.toList

-- newtype MessageQueueWrapper (q :: * -> *) a = MessageQueueWrapper { runWrapper :: (MessageQueue q a, Ord a) => ([a] -> q a) }

-- wrapAsSet :: (forall a. Ord a) => MessageQueueWrapper Set a
-- wrapAsSet = MessageQueueWrapper Set.fromList

-- wrapAsList :: (forall a. Ord a) => MessageQueueWrapper [] a
-- wrapAsList = MessageQueueWrapper identity

-- wrapAsDList :: (forall a. Ord a) => MessageQueueWrapper DList a
-- wrapAsDList = MessageQueueWrapper DList.fromList

data MessageQueueType q where
    ListMQT :: MessageQueueType []
    SetMQT :: MessageQueueType Set
    DListMQT :: MessageQueueType DList

