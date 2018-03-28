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
import qualified BatchedDequeue
import qualified Dequeue
import qualified Queue
import           Data.Digest.Murmur32
import           Microbench
import           Graph
import           RDG

buildBatchedQueue :: Int -> BatchedQueue.BatchedQueue Integer
buildBatchedQueue n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [1..n] ]
                  in BatchedQueue.buildBatchedQueue rs

buildBatchedDequeue :: Int -> BatchedDequeue.BatchedDequeue Integer
buildBatchedDequeue n = let rs = [ (toInteger . asWord32 . hash32) m | m <- [1..n] ]
                  in BatchedDequeue.buildBatchedDequeue rs

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

connectedCompF = let g = buildGraph ["A", "B", "C"] [("A", "B", "AB"), ("B", "C", "BC")]
                     cc = connectedComp g
                     v = Prelude.map vertexData $ vertices cc
                  in v

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
    ----
    putStrLn "Testing performance of 'buildDequeue'"
    microbench "buildBatchedDequeue " buildBatchedDequeue
    putStrLn "Testing performance of 'head' in pre-built dequeue"
    let rs = [ (toInteger . asWord32 . hash32) m | (m :: Integer) <- [1..1000000] ]
        batchedDequeue = BatchedDequeue.buildBatchedDequeue rs
    putStrLn $ "BatchedDequeue size: " ++ show (Dequeue.size batchedDequeue)
    --microbench "BatchedDequeue.head " $ Dequeue.head batchedDequeue

    putStrLn $ "connected comp: " ++ show (connectedCompF)





