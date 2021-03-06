{-# LANGUAGE ScopedTypeVariables #-}
{-
 AVLTreeTest.hs

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
module AVLTreeTest where

import           AVLTree
import qualified Data.Foldable        as F
import           Data.Monoid
import           Test.QuickCheck.Test

prop_test :: Bool
prop_test = let (tree :: AVLTree Integer) = buildTree [1,3,4,5]
                (tree2 :: AVLTree Integer) = insertTree 6 tree
             in (not (6 `elemTree` tree)) && (6 `elemTree` tree2)

prop_height :: [Integer] -> Bool
prop_height (xs :: [Integer]) = getAll $ treemap (\n -> All $ getHeight n == computeHeight n) $ buildTree xs


-- TODO This one is incorrect
--prop_rotations :: [Integer] -> Bool
--prop_rotations (xs :: [Integer]) = tree == (leftRotate . rightRotate) tree && tree == (rightRotate . leftRotate) tree
--                      where tree = buildTree xs

prop_insert_integer :: [Integer] -> Bool
prop_insert_integer (xs :: [Integer])  = let t = buildTree xs
                  in getAll $ F.foldMap (\x -> All $ x `elemTree` t) xs

prop_insert_float :: [Float] -> Bool
prop_insert_float (xs :: [Float])  = let t = buildTree xs
                  in getAll $ F.foldMap (\x -> All $ x `elemTree` t) xs

prop_sort :: [Integer] -> Bool
prop_sort (xs :: [Integer]) = let sorted = sort xs
                in getAll $ checkSorted sorted
                where
                  checkSorted (x:y:xs) = (All $ x < y) `mappend` checkSorted (y:xs)
                  checkSorted [] = All True
                  checkSorted (x:[]) = All True

prop_bf :: [Integer] -> Bool
prop_bf (xs :: [Integer]) = getAll $ treemap checkBf (foldr insertTree EmptyTree xs)



