-- @inline export insert arity=1
-- @inline export extract arity=1
-- @inline export fromArray arity=1
-- @inline export head always
-- @inline export size always
-- @inline export isEmpty always
-- @inline export toArray always
module Data.Heap.Internal
  ( Heap
  , insert
  , extract
  , fromArray
  , head
  , size
  , isEmpty
  , toArray
  ) where

import Prelude

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

newtype Heap a = Heap (Array a)

derive newtype instance Show a => Show (Heap a)
derive newtype instance Eq a => Eq (Heap a)

-- Time Complexity: O(log(n))
insert :: forall a. (a -> a -> Boolean) -> a -> Heap a -> Heap a
insert goal a (Heap arr') = go (Array.length arr') (Array.snoc arr' a)
  where
  go 0 arr = Heap arr
  go ix arr = do
    let p = parent ix
    if goal (getIndex arr p) (getIndex arr ix)  then
      Heap arr
    else
      go p (swap p ix arr)

-- Time Complexity: O(n)
fromArray :: forall a. (a -> a -> Boolean) -> Array a -> Heap a
fromArray goal arr = Heap (Array.foldl (heapify goal) arr (Array.range (Array.length arr / 2) 0))

-- Time Complexity: O(log(n))
extract :: forall a. (a -> a -> Boolean) -> Heap a -> Tuple (Maybe a) (Heap a)
extract goal (Heap arr') = do
  let new = Array.dropEnd 1 (swap 0 (Array.length arr' - 1) arr')
  Tuple (Array.head arr') (Heap (heapify goal new 0))

-- Takes an array and an index to a subtree which disobeys the goal heap property.
-- Returns an Array whose subtree at `ix` satisfies the goal heap property.
-- Time Complexity: O(n)
heapify :: forall a. (a -> a -> Boolean) -> Array a -> Int -> Array a
heapify goal arr ix = do
  let
    left = leftChild ix
    right = rightChild ix
    violation = getIndex arr ix
  case Array.index arr left, Array.index arr right of
    Just l, Just r | goal l r && goal l violation ->
      heapify goal (swap ix left arr) left
    Just l, Just r | goal r l && goal r violation ->
      heapify goal (swap ix right arr) right
    Just l, Nothing | goal l violation ->
      heapify goal (swap ix left arr) left
    _, _ -> arr

head :: forall a. Heap a -> Maybe a
head (Heap arr) = Array.head arr

size :: forall a. Heap a -> Int
size (Heap arr) = Array.length arr

isEmpty :: forall a. Heap a -> Boolean
isEmpty (Heap arr) = Array.null arr

toArray :: forall a. Heap a -> Array a
toArray (Heap arr) = arr

swap :: forall a. Int -> Int -> Array a -> Array a
swap a b arr = do
  let 
    tempA = getIndex arr a
    tempB = getIndex arr b
  Array.updateAtIndices [ Tuple a tempB, Tuple b tempA ] arr

getIndex :: forall a. Array a -> Int -> a
getIndex arr ix = unsafePartial (unsafeIndex arr ix)

parent :: Int -> Int
parent x | x == 0 = 0
parent x = (x - 1) / 2

leftChild :: Int -> Int
leftChild x = (2 * x) + 1

rightChild :: Int -> Int
rightChild x = (2 * x) + 2
