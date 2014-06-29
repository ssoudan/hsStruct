{-
 AVLTree.hs
 
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

module AVLTree where

import Debug.Trace
import qualified Data.Foldable as F
import Data.Monoid

-- AVL binary tree

type Height = Integer

data AVLTree a = EmptyTree 
               | Leaf a
               | Node Height a (AVLTree a) (AVLTree a)
               deriving (Show, Read, Eq)

instance F.Foldable AVLTree where
     foldMap _ EmptyTree = mempty
     foldMap f (Leaf a) = f a
     foldMap f (Node _ a l r) = F.foldMap f l `mappend` f a `mappend` F.foldMap f r

instance Functor AVLTree where
     fmap _ EmptyTree = EmptyTree
     fmap f (Leaf a) = Leaf $ f a
     fmap f (Node h a l r) = Node h (f a) (fmap f l) (fmap f r)

treemap :: (Monoid m) => (AVLTree a -> m) -> AVLTree a -> m
treemap _ EmptyTree = mempty
treemap f l@(Leaf _) = f l
treemap f n@(Node _ _ l r) = treemap f l `mappend` f n `mappend` treemap f r

singleton :: a -> AVLTree a
singleton = Leaf

getHeight :: AVLTree a -> Integer
getHeight EmptyTree = 0
getHeight (Leaf _) = 1
getHeight (Node h _ _ _) = h

computeHeight :: AVLTree a -> Integer
computeHeight EmptyTree = 0
computeHeight (Leaf _) = 1
computeHeight (Node _ _ l r) = 1 + max (computeHeight l) (computeHeight r)


buildTree :: Ord a => [a] -> AVLTree a
buildTree = foldr insertTree EmptyTree

rightRotate :: AVLTree a -> AVLTree a
rightRotate EmptyTree = EmptyTree
rightRotate l@(Leaf _) = l
rightRotate t@(Node _ _ EmptyTree _) = t
rightRotate (Node _ a (Leaf la) EmptyTree) = trace "right" (Node 2 la EmptyTree (Leaf a))
rightRotate (Node _ a (Leaf la) r) = trace "right" (
                                               let nr = Node (1 + max 1 (getHeight r)) a EmptyTree r
                                                   nh = 1 + getHeight nr
                                                in Node nh la EmptyTree nr)                                               
rightRotate (Node _ a (Node _ la ll EmptyTree) EmptyTree) = trace "right" (
                                               let nh = 1 + max 1 (getHeight ll)
                                                in Node nh la ll (Leaf a))
rightRotate (Node _ a (Node _ la ll lr) r) = trace "right" (
                                               let nr = Node (1 + max (getHeight lr) (getHeight r)) a lr r
                                                   nh = 1 + max (getHeight nr) (getHeight ll)
                                                in Node nh la ll nr)


leftRotate :: AVLTree a -> AVLTree a
leftRotate (Node _ a EmptyTree (Leaf ra)) = trace "left" (Node 2 ra (Leaf a) EmptyTree)
leftRotate (Node _ a l (Leaf ra)) = trace "left" (
                                              let nl = Node (1 + getHeight l) a l EmptyTree
                                                  nh = 1 + getHeight nl
                                               in Node nh ra nl EmptyTree)
leftRotate (Node _ a EmptyTree (Node _ ra EmptyTree rr)) = trace "left" (
                                              let nh = 1 + max 1 (getHeight rr)
                                               in Node nh ra (Leaf a) rr) 
leftRotate (Node _ a l (Node _ ra rl rr)) = trace "left" (
                                              let nl = Node (1 + max (getHeight l) (getHeight rl)) a l rl
                                                  nh = 1 + max (getHeight nl) (getHeight rr)
                                               in Node nh ra nl rr)                                               
                                               
-- leftRotate_cond :: AVLTree a -> AVLTree a
leftRotate_cond n@(Node h _ l r) = let hl = getHeight l
                                       hr = getHeight r
                                       adbf = hl-hr
                                     in case bf of -1 -> leftRotate n
                                                   otherwise -> n
-- rightRotate_cond :: AVLTree a -> AVLTree a
rightRotate_cond n@(Node h _ l r) = let hl = getHeight l
                                        hr = getHeight r
                                        bf = hl-hr
                                     in case bf of 1 -> rightRotate n
                                                   otherwise -> n


-- insertTree a -> AVLTree a -> AVLTree a
insertTree x EmptyTree = singleton x
insertTree x (Leaf a) 
    | x == a = Leaf a
    | x < a = Node 2 x EmptyTree (Leaf a)    
    | x > a = Node 2 x (Leaf a) EmptyTree
insertTree x (Node height a left right)
    | x == a = Node height a left right
    | x < a = create_node a (insertTree x left) right
    | x > a = create_node a left (insertTree x right)

-- create_node :: a -> AVLTree a -> AVLTree a -> AVLTree a
create_node a l r = let hr = getHeight r
                        hl = getHeight l
                        bf = hl-hr
                        nh = 1 + max hl hr
                     in case () of _ 
                                    | bf <= -2 -> leftRotate (Node nh a l (rightRotate_cond r))
                                    | bf >= 2 -> rightRotate (Node nh a (leftRotate_cond l) r)
                                    | otherwise -> Node nh a l r


-- elemTree :: a -> AVLTree a -> Bool
elemTree x EmptyTree = False
elemTree x (Leaf a) = x == a
elemTree x (Node _ a l r) 
                  | x == a = True
                  | x < a = elemTree x l
                  | x > a = elemTree x r


-- checkBf :: AVLTree a -> All Bool
checkBf EmptyTree      = All True
checkBf (Leaf _)       = All True
checkBf (Node _ _ l r) = All $ abs ((getHeight l) - (getHeight r)) < 2

-- sort :: [a] -> [a]
sort xs = F.foldMap (\a -> [a]) $ buildTree xs



--main = do 
--         seed <- getStdGen
--         let elems = take 20 $ randomRs (0,1000) seed :: [Integer]
--         print $ buildTree elems
--         print $ prop_height elems
--         print $ prop_bf elems
--         print $ testRotations elems
         
     


