{-
 BinomialHeap.hs

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
module BinomialHeap where

import           Heap

data Tree a = Node a [Tree a]

data RankedTree  a = RankedTree { getRank :: Int
                                , getTree :: Tree a
                                }

data BinomialHeap a = BinomialHeap [(RankedTree a)]


-- link two trees of the same order into one of higher order
link :: Ord a => RankedTree a -> RankedTree a -> RankedTree a
link (RankedTree r1 t1@(Node a1 t1s)) (RankedTree r2 t2@(Node a2 t2s)) = if (a1 <= a2) then
                                                                            RankedTree (r1 + 1) (Node a1 (t2:t1s))
                                                                         else
                                                                            RankedTree (r2 + 1) (Node a2 (t1:t2s))

-- Merge two heaps
merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge a@(BinomialHeap _ )   (BinomialHeap []) = a
merge   (BinomialHeap []) b@(BinomialHeap _ ) = b
merge   (BinomialHeap xs)   (BinomialHeap ys) = BinomialHeap (mergeTrees xs ys)

mergeTrees :: Ord a => [(RankedTree a)] -> [(RankedTree a)] -> [(RankedTree a)]
mergeTrees [] b = b
mergeTrees a [] = a
mergeTrees a@(x:xs) b@(y:ys) = let rankX = getRank x
                                   rankY = getRank y
                                in case () of _ | rankX == rankY -> -- here we need to check who will become the 
                                                                    -- 'leaf', then 'link' the 2 trees, next check 
                                                                    -- if there is some room or it will collide and
                                                                    -- require to apply the merge again with the 
                                                                    -- tails
                                                                    let linked = link x y 
                                                                        newRank = getRank linked
                                                                        nextRankX = getRank . head $ xs
                                                                        nextRankY = getRank . head $ ys
                                                                     in if newRank /= nextRankX then 
                                                                            if newRank /= nextRankY then
                                                                                linked:(mergeTrees xs ys)
                                                                            else
                                                                                mergeTrees (linked:xs) ys
                                                                        else
                                                                            if newRank /= nextRankY then
                                                                                mergeTrees xs (linked:ys)
                                                                            else
                                                                                linked:(mergeTrees xs ys)

                                                | rankX < rankY  -> (x:(mergeTrees xs b))
                                                | otherwise      -> (y:(mergeTrees a  ys))


-- [http://en.wikipedia.org/wiki/Binomial_heap]
--
-- A binomial tree is defined recursively:
-- - A binomial tree of order 0 is a single node
-- - A binomial tree of order k has a root node whose children are roots of 
--   binomial trees of orders k−1, k−2, ..., 2, 1, 0 (in this order).
--
-- Because of its unique structure, a binomial tree of order k can be 
-- constructed from two trees of order k−1 trivially by attaching one of 
-- them as the leftmost child of root of the other one. This feature is 
-- central to the merge operation of a binomial heap, which is its major 
-- advantage over other conventional heaps.

-- P1: Each binomial tree in a heap obeys the minimum-heap property: the key 
--     of a node is greater than or equal to the key of its parent.
-- P2: There can only be either one or zero binomial trees for each order, 
--     including zero order.
instance Heap BinomialHeap where
    -- empty :: h a
    empty = (BinomialHeap [])
    -- isEmpty :: h a -> Bool
    isEmpty (BinomialHeap h) = null h
    -- insert :: Ord a => a -> h a -> h a
    insert = undefined
    -- merge :: Ord a => h a -> h a -> h a
    merge = BinomialHeap.merge
    -- findMin :: Ord a => h a -> a
    findMin = undefined
    -- deleteMin :: Ord a => h a -> h a
    deleteMin = undefined

