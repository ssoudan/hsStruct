{-
 Heap.hs
 
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
module Heap where


class Heap h where
    empty :: h a
    isEmpty :: h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

