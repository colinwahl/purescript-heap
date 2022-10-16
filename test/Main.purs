module Test.Main where

import Prelude

import Control.Monad.State.Class (get, modify, put)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Heap.Min as MinHeap
import Data.Heap.Max as MaxHeap
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Uncurried.StateT (execStateT)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "MinHeap" do
    it "Can insert" do
      let s = MinHeap.insert 10 mempty
      MinHeap.toArray s `shouldEqual` [ 10 ]
    it "Can insert & extract" do
      let s = MinHeap.insert 10 mempty
      MinHeap.extractMin s `shouldEqual` Tuple (Just 10) mempty
    it "Can insert in increasing order & extract" $ void $ execStateT mempty do
      for_ (Array.range 0 1000) \i -> do
        modify (MinHeap.insert i)
      for_ (Array.range 0 1000) \i -> do
        s <- get
        let Tuple m next = MinHeap.extractMin s
        put next
        m `shouldEqual` Just i
      s <- get
      MinHeap.extractMin s `shouldEqual` Tuple Nothing mempty
    it "Can insert in decreasing order & extract" $ void $ execStateT mempty do
      for_ (Array.range 1000 0) \i -> do
        modify (MinHeap.insert i)
      for_ (Array.range 0 1000) \i -> do
        s <- get
        let Tuple m next = MinHeap.extractMin s
        put next
        m `shouldEqual` Just i
      s <- get
      MinHeap.extractMin s `shouldEqual` Tuple Nothing mempty
    it "Can create a MinHeap from an Array" $ void $ execStateT (MinHeap.fromArray (Array.range 1000 0)) do
      for_ (Array.range 0 1000) \i -> do
        s <- get
        let Tuple m next = MinHeap.extractMin s
        put next
        m `shouldEqual` Just i
      s <- get
      MinHeap.extractMin s `shouldEqual` Tuple Nothing mempty
    it "Can combine two MinHeaps with append" $ void $ execStateT (MinHeap.fromArray (Array.range 1000 0) <> MinHeap.fromArray (Array.range 1001 2000)) do
      for_ (Array.range 0 2000) \i -> do
        s <- get
        let Tuple m next = MinHeap.extractMin s
        put next
        m `shouldEqual` Just i
      s <- get
      MinHeap.extractMin s `shouldEqual` Tuple Nothing mempty
  describe "MaxHeap" do
    it "Can insert" do
      let s = MaxHeap.insert 10 mempty
      MaxHeap.toArray s `shouldEqual` [ 10 ]
    it "Can insert & extract" do
      let s = MaxHeap.insert 10 mempty
      MaxHeap.extractMax s `shouldEqual` Tuple (Just 10) mempty
    it "Can insert in increasing order & extract" $ void $ execStateT mempty do
      for_ (Array.range 0 1000) \i -> do
        modify (MaxHeap.insert i)
      for_ (Array.range 1000 0) \i -> do
        s <- get
        let Tuple m next = MaxHeap.extractMax s
        put next
        m `shouldEqual` Just i
      s <- get
      MaxHeap.extractMax s `shouldEqual` Tuple Nothing mempty
    it "Can insert in decreasing order & extract" $ void $ execStateT mempty do
      for_ (Array.range 1000 0) \i -> do
        modify (MaxHeap.insert i)
      for_ (Array.range 1000 0) \i -> do
        s <- get
        let Tuple m next = MaxHeap.extractMax s
        put next
        m `shouldEqual` Just i
      s <- get
      MaxHeap.extractMax s `shouldEqual` Tuple Nothing mempty
    it "Can create a MaxHeap from an Array" $ void $ execStateT (MaxHeap.fromArray (Array.range 1000 0)) do
      for_ (Array.range 1000 0) \i -> do
        s <- get
        let Tuple m next = MaxHeap.extractMax s
        put next
        m `shouldEqual` Just i
      s <- get
      MaxHeap.extractMax s `shouldEqual` Tuple Nothing mempty
    it "Can combine two MaxHeaps with append" $ void $ execStateT (MaxHeap.fromArray (Array.range 1000 0) <> MaxHeap.fromArray (Array.range 1001 2000)) do
      for_ (Array.range 2000 0) \i -> do
        s <- get
        let Tuple m next = MaxHeap.extractMax s
        put next
        m `shouldEqual` Just i
      s <- get
      MaxHeap.extractMax s `shouldEqual` Tuple Nothing mempty
