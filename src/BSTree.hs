{-
 BSTree.hs

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
module BSTree where

import qualified Data.Foldable as F
import           Data.Monoid

data BSTree a = EmptyTree
                | Node a (BSTree a) (BSTree a)


instance F.Foldable BSTree where
     foldMap _ EmptyTree = mempty
     foldMap f (Node a l r) = F.foldMap f l `mappend` f a `mappend` F.foldMap f r

instance Functor BSTree where
     fmap _ EmptyTree = EmptyTree
     fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)


insertTree :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
insertTree a EmptyTree = Node a EmptyTree EmptyTree
insertTree a n@(Node v l r) | v == a = n
                            | v > a = Node v (insertTree a l) r
                            | v < a = Node v l (insertTree a r)

buildTree :: Ord a => [a] -> BSTree a
buildTree = foldr insertTree EmptyTree

elemTree :: (Eq a, Ord a) => a -> BSTree a -> Bool
elemTree _ EmptyTree = False
elemTree a (Node v l r) | v == a = True
                        | v > a = elemTree a l
                        | v < a = elemTree a r

sort :: Ord a => [a] -> [a]
sort xs = F.foldMap (: []) $ buildTree xs

size :: BSTree a -> Integer
size a = F.sum $ fmap (const 1) a
