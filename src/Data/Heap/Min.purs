module Data.Heap.Min
  ( MinHeap
  , insert
  , findMin
  , extractMin
  , fromArray
  , toArray
  , size
  , isEmpty
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Heap.Internal as Internal

goal :: forall a. Ord a => a -> a -> Boolean
goal a b = a < b

newtype MinHeap a = MinHeap (Internal.Heap a)

derive newtype instance Show a => Show (MinHeap a)
derive newtype instance Eq a => Eq (MinHeap a)

-- Time Complexity: O(n)
instance Ord a => Semigroup (MinHeap a) where
  append a b = fromArray (toArray a <> toArray b)

-- Time Complexity: O(1)
instance Ord a => Monoid (MinHeap a) where
  mempty = MinHeap (Internal.fromArray goal [])

size :: forall a. MinHeap a -> Int
size (MinHeap heap) = Internal.size heap

isEmpty :: forall a. MinHeap a -> Boolean
isEmpty (MinHeap heap) = Internal.isEmpty heap

-- Time Complexity: O(1)
findMin :: forall a. MinHeap a -> Maybe a
findMin (MinHeap heap) = Internal.head heap

-- Time Complexity: O(1)
toArray :: forall a. MinHeap a -> Array a
toArray (MinHeap heap) = Internal.toArray heap

-- Time Complexity: O(log(n))
insert :: forall a. Ord a => a -> MinHeap a -> MinHeap a
insert a (MinHeap heap) = MinHeap (Internal.insert goal a heap)

-- Time Complexity: O(n)
fromArray :: forall a. Ord a => Array a -> MinHeap a
fromArray = MinHeap <<< Internal.fromArray goal

-- Time Complexity: O(log(n))
extractMin :: forall a. Ord a => MinHeap a -> Tuple (Maybe a) (MinHeap a)
extractMin (MinHeap heap) = map MinHeap (Internal.extract goal heap)
