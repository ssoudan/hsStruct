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

import qualified Data.Foldable as F
import           Data.List     (delete, minimumBy, sort)
import           Data.Monoid
import           Heap


data Tree a = Node a [Tree a] deriving Show

getValue :: Tree a -> a
getValue (Node a _) = a

data RankedTree a = RankedTree { getRank  :: Int
                                , getTree :: Tree a
                                } deriving Show

data BinomialHeap a = BinomialHeap [(RankedTree a)] deriving Show

getRankedTrees :: Ord a => BinomialHeap a -> [RankedTree a]
getRankedTrees (BinomialHeap ts) = ts

-- for our purpose comparing on the rank is enough since there will be only one tree for each rank at max
instance Eq (RankedTree a) where
    (RankedTree rk1 _) == (RankedTree rk2 _) = rk1 == rk2


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

hrank :: [RankedTree a] -> Int
hrank [] = 0
hrank xs = getRank . head $ xs

mergeTrees :: Ord a => [(RankedTree a)] -> [(RankedTree a)] -> [(RankedTree a)]
mergeTrees [] b = b
mergeTrees a [] = a
mergeTrees a@(x:xs) b@(y:ys) =  let rankX = getRank x
                                    rankY = getRank y
                                in case () of _ | rankX == rankY -> -- here we need to check who will become the
                                                                    -- 'leaf', then 'link' the 2 trees, next check
                                                                    -- if there is some room or it will collide and
                                                                    -- require to apply the merge again with the
                                                                    -- tails; note that in case both xs and ys already
                                                                    -- have a BT of order k+1, we can just prepend the
                                                                    -- linked BT and let those two BTs be linked to
                                                                    -- a BT of order k+2 without breaking P2.
                                                                    let linked = link x y
                                                                        newRank = getRank linked
                                                                        nextRankX = hrank xs
                                                                        nextRankY = hrank ys
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


btFromList :: Ord a => [a] -> BinomialHeap a
btFromList xs = foldr Heap.insert empty xs

singleton :: Ord a => a -> BinomialHeap a
singleton a = (BinomialHeap [RankedTree 0 (Node a [])])


deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin (BinomialHeap []) = error "undefined"
deleteMin (BinomialHeap ts) = let r@(RankedTree rk (Node _ sts)) = minimumBy (\tt tt2 -> compare (getValue . getTree $ tt) (getValue . getTree $ tt2)) ts
                                  subtrees = BinomialHeap (zipWith (\rr tt -> (RankedTree rr tt)) [0,1..(rk-1)] (reverse sts))
                                  withoutR = BinomialHeap (delete r ts)
                               in BinomialHeap.merge withoutR subtrees

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
    -- empty :: Ord a => h a
    empty = (BinomialHeap [])
    -- isEmpty :: Ord a => h a -> Bool
    isEmpty (BinomialHeap h) = null h
    -- insert :: Ord a => a -> h a -> h a
    insert a h = BinomialHeap.merge (singleton a) h
    -- merge :: Ord a => h a -> h a -> h a
    merge = BinomialHeap.merge
    -- findMin :: Ord a => h a -> a
    -- TODO this implementation is not O(1)
    findMin (BinomialHeap []) = error "undefined"
    findMin (BinomialHeap ts) = minimum $ map (\t ->  getValue . getTree $ t) ts
    -- deleteMin :: Ord a => h a -> h a
    deleteMin = BinomialHeap.deleteMin



--
-- Because findMin is O(log n) in BinomialHeap, we wrap it in SavedMinBinomialHeap which recall the value of minimum
-- or compute it across already complex operations (O(log n)) so that findMin becomes O(1)
--
data SavedMinBinomialHeap a = SavedMinBinomialHeap { getMin :: a, getHeap :: BinomialHeap a }
                            | EmptyHeap deriving Show

s2b :: Ord a => SavedMinBinomialHeap a -> BinomialHeap a
s2b EmptyHeap = Heap.empty

b2s :: Ord a => a -> BinomialHeap a -> SavedMinBinomialHeap a
b2s m h = SavedMinBinomialHeap m h

fromList :: Ord a => [a] -> SavedMinBinomialHeap a
fromList xs = let h = btFromList xs
               in if (isEmpty h) then
                    EmptyHeap
                  else
                    b2s (Heap.findMin h) h

instance Heap SavedMinBinomialHeap where

    -- O(1)
    -- empty :: Ord a => h a
    empty = EmptyHeap

    -- O(1)
    -- isEmpty :: Ord a => h a -> Bool
    isEmpty EmptyHeap = True
    isEmpty _ = False

    -- O(1) amortized (see [Okasaki p. 45])
    -- O(log n) worst case
    -- insert :: Ord a => a -> h a -> h a
    insert a EmptyHeap = b2s a (BinomialHeap.merge (singleton a) (Heap.empty))
    insert a (SavedMinBinomialHeap m t) = b2s (min a m) (BinomialHeap.merge (singleton a) t)

    -- O(log n)
    -- merge :: Ord a => h a -> h a -> h a
    merge EmptyHeap (SavedMinBinomialHeap m t)= b2s m t
    merge (SavedMinBinomialHeap m t) EmptyHeap = b2s m t
    merge (SavedMinBinomialHeap m t) (SavedMinBinomialHeap m2 t2) = b2s (min m m2) (BinomialHeap.merge t t2)

    -- O(1)
    -- findMin :: Ord a => h a -> a
    findMin (EmptyHeap) = error "undefined"
    findMin (SavedMinBinomialHeap m _) = m

    -- O(log n)
    -- deleteMin :: Ord a => h a -> h a
    deleteMin (EmptyHeap) = error "undefined"
    deleteMin (SavedMinBinomialHeap _ t) = let t' = BinomialHeap.deleteMin t
                                            in if (Heap.isEmpty t') then
                                                EmptyHeap
                                             else
                                                let m' = Heap.findMin t'
                                                 in b2s m' t'

--
-- Check properties on the heap
--
checkP1OnATree :: Ord a => a -> Tree a -> Bool
checkP1OnATree m (Node a []) = a >= m
checkP1OnATree m (Node a (t:ts)) = a >= m && checkP1OnATree a t && childOk
                            where childOk = getAll $ F.foldMap (All . (checkP1OnATree a)) ts

checkP1OnSavedMinBinomialHeap :: Ord a => SavedMinBinomialHeap a -> Bool
checkP1OnSavedMinBinomialHeap EmptyHeap = True
checkP1OnSavedMinBinomialHeap (SavedMinBinomialHeap m (BinomialHeap ts)) = getAll $ F.foldMap (\(RankedTree _ t) -> All $ checkP1OnATree m t) ts

checkP2OnSavedMinBinomialHeap :: Ord a => SavedMinBinomialHeap a -> Bool
checkP2OnSavedMinBinomialHeap EmptyHeap = True
checkP2OnSavedMinBinomialHeap (SavedMinBinomialHeap _ (BinomialHeap ts)) = let ranks = sort $ map getRank ts
                                                                            in failOnDuplicate (head ranks) (tail ranks)
                                                                         where failOnDuplicate _ [] = True
                                                                               failOnDuplicate a (tt:tts) = a /= tt && failOnDuplicate tt tts
