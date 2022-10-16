module Data.Heap.Max
  ( MaxHeap
  , insert
  , findMax
  , extractMax
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
goal a b = b < a

newtype MaxHeap a = MaxHeap (Internal.Heap a)

derive newtype instance Show a => Show (MaxHeap a)
derive newtype instance Eq a => Eq (MaxHeap a)

-- Time Complexity: O(n)
instance Ord a => Semigroup (MaxHeap a) where
  append a b = fromArray (toArray a <> toArray b)

-- Time Complexity: O(1)
instance Ord a => Monoid (MaxHeap a) where
  mempty = MaxHeap (Internal.fromArray goal [])

size :: forall a. MaxHeap a -> Int
size (MaxHeap heap) = Internal.size heap

isEmpty :: forall a. MaxHeap a -> Boolean
isEmpty (MaxHeap heap) = Internal.isEmpty heap

-- Time Complexity: O(1)
findMax :: forall a. MaxHeap a -> Maybe a
findMax (MaxHeap heap) = Internal.head heap

-- Time Complexity: O(1)
toArray :: forall a. MaxHeap a -> Array a
toArray (MaxHeap heap) = Internal.toArray heap

-- Time Complexity: O(log(n))
insert :: forall a. Ord a => a -> MaxHeap a -> MaxHeap a
insert a (MaxHeap heap) = MaxHeap (Internal.insert goal a heap)

-- Time Complexity: O(n)
fromArray :: forall a. Ord a => Array a -> MaxHeap a
fromArray = MaxHeap <<< Internal.fromArray goal

-- Time Complexity: O(log(n))
extractMax :: forall a. Ord a => MaxHeap a -> Tuple (Maybe a) (MaxHeap a)
extractMax (MaxHeap heap) = map MaxHeap (Internal.extract goal heap)
