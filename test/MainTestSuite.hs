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
        ]