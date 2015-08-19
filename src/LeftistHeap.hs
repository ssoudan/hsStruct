{-
 LeftistHeap.hs

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
module LeftistHeap where

import           Data.List (intersperse)
import           Heap


data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

-- Rank is the length of the /right spine/ (ie rightmost path to an empty node)
--
computeRank :: LeftistHeap a -> Int
computeRank E = 0
computeRank (T _ _ _ r) = 1 + computeRank r

-- P1: Leftist heap ensures rank of any left child is at least as large of the rank
--     of its right sibling.
--
-- see LeftistHeapTest
checkP1OnNode :: LeftistHeap a -> Bool
checkP1OnNode E = True
checkP1OnNode (T _ _ l r) = computeRank l >= computeRank r

checkP1OnLeftistHeap :: LeftistHeap a -> Bool
checkP1OnLeftistHeap E = True
checkP1OnLeftistHeap (T _ _ l r) = checkP1OnNode l && checkP1OnNode r && checkP1OnLeftistHeap l && checkP1OnLeftistHeap r 

-- Return the rank of a LeftistHeap
rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r


-- Compute the rank and potentially swap the children if necessary
-- remember P1 and the definition of the rank.
makeT :: Ord a => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT a h1 h2 = let rank1 = rank h1
                    rank2 = rank h2
                 in if rank1 >= rank2 then
                        T (rank2 + 1) a h1 h2
                    else
                        T (rank1 + 1) a h2 h1


-- Merge 2 LeftistHeaps
--
-- > merge (makeT 2 E E) (makeT 1 E E)
-- T 1 1 (T 1 2 E E) E
-- > merge (makeT 3 E E) (merge (makeT 2 E E) (makeT 1 E E))
--
merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge E h = h
merge h E = h
merge h1@(T _ a1 l1 r1) h2@(T _ a2 l2 r2) = if (a2 >= a1) then
                                                    makeT a1 l1 (LeftistHeap.merge r1 h2)
                                                else
                                                    makeT a2 l2 (LeftistHeap.merge h1 r2)


-- Apply f on a LeftistHeap in a BFS manner
bfsMap :: ((LeftistHeap a) -> b) -> LeftistHeap a -> [[b]]
bfsMap f t = tbf f [t]
        where 
              -- auxilary function to apply f on a LeftistHeap in a BFS manner
              tbf :: ((LeftistHeap a) -> b) -> [LeftistHeap a] -> [[b]]
              tbf _ [] = []
              tbf ff xs = (mapOnRoot ff xs) : (tbf ff (concat (map extractChildren xs)))
              -- apply a function on the element stored in a node
              mapOnRoot :: ((LeftistHeap a) -> b) -> [LeftistHeap a] -> [b]
              mapOnRoot _ [] = []
              mapOnRoot fff (x_:xs_) = (fff x_):(mapOnRoot fff xs_)
              -- extract children of a node
              extractChildren :: LeftistHeap a -> [LeftistHeap a]
              extractChildren E = []
              extractChildren (T _ _ l r) = [l,r]


instance Show a => Show (LeftistHeap a) where
    show t = unlines $ map (concat . (Data.List.intersperse " ")) $ bfsMap show2 t
            where
                show2 :: Show a => LeftistHeap a -> String
                show2 E = " "
                show2 (T _ a l r) = (fill ll) ++ showA ++ (fill lr)
                                where
                                    ll = treeWidth l
                                    lr = treeWidth r
                                    showA = show a
                                    fill x = replicate x ' '
                treeWidth :: (Show a) => LeftistHeap a -> Int
                treeWidth E = 1
                treeWidth (T _ a l r) = treeWidth l + treeWidth r + (length . show) a


-- Leftist Heap implementation of a heap.
-- see [Okasaki, p. 17]
--
-- O(1): empty
-- O(1): isEmpty
-- O(1): findMin
-- O(log n): insert
-- O(log n): merge
-- O(log n): deleteMin
instance Heap LeftistHeap where
    -- empty :: Ord a => LeftistHeap a
    empty = E
    -- isEmpty :: Ord a => LeftistHeap a -> Bool
    isEmpty E = True
    isEmpty _ = False
    -- insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
    insert a t = LeftistHeap.merge (makeT a E E) t
    -- merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    merge = LeftistHeap.merge
    -- findMin :: Ord a => LeftistHeap a -> a
    findMin E = error "undefined"
    findMin (T _ a _ _) = a
    -- deleteMin :: Ord a => LeftistHeap a -> LeftistHeap a
    deleteMin E = error "undefined"
    deleteMin (T _ _ l r) = LeftistHeap.merge l r


-- *LeftistHeap> print $ Heap.merge (LeftistHeap.makeT 6 E E) $ Heap.merge (LeftistHeap.makeT 1 E E) $ Heap.merge (LeftistHeap.makeT 4 E E) (Heap.merge (LeftistHeap.makeT 2 E E) (LeftistHeap.makeT 5 E E))
--        1   
--    2     6 
--  5   4     
       

-- *LeftistHeap> print $ Heap.merge (LeftistHeap.makeT 3 E E) (Heap.merge (LeftistHeap.makeT 7 E E) (LeftistHeap.makeT 9 E E))
--      3 
--    7   
--  9   

-- *LeftistHeap> print $ Heap.merge (Heap.merge (LeftistHeap.makeT 6 E E) $ Heap.merge (LeftistHeap.makeT 1 E E) $ Heap.merge (LeftistHeap.makeT 4 E E) (Heap.merge (LeftistHeap.makeT 2 E E) (LeftistHeap.makeT 5 E E))) (Heap.merge (LeftistHeap.makeT 3 E E) (Heap.merge (LeftistHeap.makeT 7 E E) (LeftistHeap.makeT 9 E E)))
--        1         
--    2         3   
--  5   4     7   6 
--          9       
   

