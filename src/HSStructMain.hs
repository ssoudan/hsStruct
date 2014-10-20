{-# LANGUAGE ScopedTypeVariables #-}
{-
 HSStructMain.hs
 
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

module Main where

import qualified AVLTree
import qualified BSTree
import qualified BatchedQueue
import qualified Queue
import           Data.Digest.Murmur32
import           Microbench

--sum1 :: Int -> Int
--sum1 n = sum [1..n]

--sum2 :: Int -> Int
--sum2 n = foldl (+) 0 [1..n]

buildBatchedQueue :: Int -> BatchedQueue.BatchedQueue Integer
buildBatchedQueue n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [1..n] ]
                  in BatchedQueue.buildBatchedQueue rs

buildAVLTree :: Int -> AVLTree.AVLTree Integer
buildAVLTree n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [1..n] ]
                  in AVLTree.buildTree rs

searchAVLTree :: AVLTree.AVLTree Integer -> Int -> Integer
searchAVLTree tree n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [500000..n] ]
                           (a,b) = span (`AVLTree.elemTree` tree) rs
                        in sum a + sum b

buildBSTree :: Int -> BSTree.BSTree Integer
buildBSTree n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [1..n] ]
                 in BSTree.buildTree rs

searchBSTree :: BSTree.BSTree Integer -> Int -> Integer
searchBSTree tree n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [500000..n] ]
                          (a,b) = span (`BSTree.elemTree` tree) rs
                       in sum a + sum b

main :: IO ()
main = do
    putStrLn "Testing performance of 'buildTree'"
    microbench "buildAVLTree " buildAVLTree
    microbench "buildBSTree " buildAVLTree
    putStrLn "Testing performance of 'elemTree' in pre-built trees"
    let rs = [ (toInteger . asWord32 . hash32) m | (m :: Integer) <- [1..1000000] ]
        avltree = AVLTree.buildTree rs
        bstree = BSTree.buildTree rs
    putStrLn $ "AVLTree size: " ++ show (AVLTree.size avltree)
    putStrLn $ "BSTree size: " ++ show (BSTree.size bstree)
    microbench "searchAVLTree " $ searchAVLTree avltree
    microbench "searchBSTree " $ searchBSTree bstree
    ----
    putStrLn "Testing performance of 'buildQueue'"
    microbench "buildBatchedQueue " buildBatchedQueue
    putStrLn "Testing performance of 'head' in pre-built queue"
    let rs = [ (toInteger . asWord32 . hash32) m | (m :: Integer) <- [1..1000000] ]
        batchedQueue = BatchedQueue.buildBatchedQueue rs
    putStrLn $ "BatchedQueue size: " ++ show (Queue.size batchedQueue)
    --microbench "BatchedQueue.head " $ Queue.head batchedQueue