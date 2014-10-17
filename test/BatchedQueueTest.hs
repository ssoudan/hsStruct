{-# LANGUAGE ScopedTypeVariables #-}
{-
 QueueTest.hs

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
module BatchedQueueTest where

import           Queue
import           BatchedQueue
import qualified Data.Foldable        as F
import           Data.Monoid
import           Test.QuickCheck.Test

prop_test :: Bool
prop_test = let (q :: BatchedQueue Integer) = buildBatchedQueue [1,3,4,5]
                (q2 :: BatchedQueue Integer) = snoc q 6
             in (Queue.head q2 == 1) && (Queue.head q == 1)

prop_empty :: Bool
prop_empty = Queue.isEmpty (Queue.empty :: BatchedQueue Integer) 


prop_build :: [Integer] -> Bool
prop_build xs = let (q :: BatchedQueue Integer) = buildBatchedQueue xs
                 in F.toList q == xs

prop_head_tail :: [Integer] -> Bool
prop_head_tail [] = True
prop_head_tail [x] = True
prop_head_tail (x:xs) = let (q :: BatchedQueue Integer) = buildBatchedQueue (x:xs)
                         in (Queue.head q == x) && (F.toList (Queue.tail q) == xs)



