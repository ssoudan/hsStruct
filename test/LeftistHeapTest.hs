{-
 LeftistHeapTest.hs
 
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
module LeftistHeapTest where

import           LeftistHeap
import           Heap
import           Test.QuickCheck.Test

prop_empty :: Bool
prop_empty = isEmpty (empty :: LeftistHeap a)

prop_insert_not_empty :: Ord a => [a] -> Bool
prop_insert_not_empty xs = if (null xs) then 
                            True
                           else 
                            not . isEmpty $ foldr insert (empty :: LeftistHeap a) xs

prop_findMin :: Ord a => [a] -> Bool
prop_findMin xs = if (null xs) then 
                    True
                  else 
                    let m = minimum xs
                    in m == (findMin $ foldr insert (empty :: LeftistHeap a) xs)

prop_merge_findMin :: Ord a => [a] -> [a] -> Bool
prop_merge_findMin xs ys = if (null xs) || (null ys) then 
                            True
                           else 
                            let m = minimum (xs ++ ys)
                            in m == (findMin $ Heap.merge (foldr insert (empty :: LeftistHeap a) xs) (foldr insert (empty :: LeftistHeap a) ys))

prop_P1 :: Ord a => [a] -> Bool
prop_P1 xs = let t = foldr insert (empty :: LeftistHeap a) xs
              in checkP1OnLeftistHeap t
            