{-
 MainTestSuite.hs

 Copyright 2014 Sebastien Soudan

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

-}
module Main (
    main
 ) where

import qualified AVLTreeTest
import qualified BSTreeTest
import qualified BatchedQueueTest
import qualified BatchedDequeueTest
import qualified LeftistHeapTest
import qualified BinomialHeapTest
import qualified RDGTest

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import Test.Framework.Options

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [
          testGroup "AVLTree: simple"
            [ testProperty "insert" AVLTreeTest.prop_test
            --, testProperty "rotations" prop_rotations
            , testProperty "insert - Integer" AVLTreeTest.prop_insert_integer
            , testProperty "insert - Float" AVLTreeTest.prop_insert_float
            ]
        , testGroup "AVLTree: complex"
            [ testProperty "Height" AVLTreeTest.prop_height
            , testProperty "Balance factor" AVLTreeTest.prop_bf
            , testProperty "Sort" AVLTreeTest.prop_sort
            ]
        , testGroup "BSTree: simple"
            [ testProperty "insert" BSTreeTest.prop_test
            , testProperty "insert - Integer" BSTreeTest.prop_insert_integer
            , testProperty "insert - Float" BSTreeTest.prop_insert_float
            ]
        , testGroup "BSTree: complex"
            [ testProperty "Sort" BSTreeTest.prop_sort
            ]
        , testGroup "BatchedQueue: simple"
            [ testProperty "insert" BatchedQueueTest.prop_test
            , testProperty "build" BatchedQueueTest.prop_build
            , testProperty "empty isEmpty" BatchedQueueTest.prop_empty
            , testProperty "head - tail" BatchedQueueTest.prop_head_tail
            ]
        , testGroup "BatchedDequeue: simple"
            [ testProperty "insert" BatchedDequeueTest.prop_test
            , testProperty "build" BatchedDequeueTest.prop_build
            , testProperty "empty isEmpty" BatchedDequeueTest.prop_empty
            , testProperty "head - tail" BatchedDequeueTest.prop_head_tail
            ]
        , testGroup "LeftistHeap: simple"
            [ testProperty "empty isEmpty" LeftistHeapTest.prop_empty
            , testProperty "findMin" (LeftistHeapTest.prop_findMin :: [Int] -> Bool)
            , testProperty "merge - findMin" (LeftistHeapTest.prop_merge_findMin :: [Int] -> [Int] -> Bool)
            , testProperty "deleteMin" (LeftistHeapTest.prop_deleteMin :: [Int] -> Bool)
            , testProperty "insert" (LeftistHeapTest.prop_insert_not_empty :: [Int] -> Bool)
            , testProperty "P1" (LeftistHeapTest.prop_P1 :: [Int] -> Bool)
            ]
        , testGroup "SavedMinBinomialHeap: simple"
            [ testProperty "empty isEmpty" BinomialHeapTest.prop_empty
            , testProperty "findMin" (BinomialHeapTest.prop_findMin :: [Int] -> Bool)
            , testProperty "merge - findMin" (BinomialHeapTest.prop_merge_findMin :: [Int] -> [Int] -> Bool)
            , testProperty "deleteMin" (BinomialHeapTest.prop_deleteMin :: [Int] -> Bool)
            , testProperty "insert" (BinomialHeapTest.prop_insert_not_empty :: [Int] -> Bool)
            , testProperty "P1" (BinomialHeapTest.prop_P1 :: [Int] -> Bool)
            , testProperty "P2" (BinomialHeapTest.prop_P2 :: [Int] -> Bool)
            ]
        , testGroup "RDG: simple"
            [ testProperty "empty graph isEmpty" RDGTest.prop_empty
            ]
        ]
