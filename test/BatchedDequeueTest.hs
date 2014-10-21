{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      :  BatchedDequeueTest
Description :  ... 
Copyright   :  (c) Sebastien Soudan 
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module BatchedDequeueTest where

import           Dequeue
import           BatchedDequeue
import qualified Data.Foldable        as F
import           Data.Monoid
import           Test.QuickCheck.Test


prop_test :: Bool
prop_test = let (q :: BatchedDequeue Integer) = buildBatchedDequeue [1,3,4,5]
                (q2 :: BatchedDequeue Integer) = snoc q 6
             in (Dequeue.head q2 == 1) && (Dequeue.head q == 1) 

prop_empty :: Bool
prop_empty = Dequeue.isEmpty (Dequeue.empty :: BatchedDequeue Integer) 


prop_build :: [Integer] -> Bool
prop_build xs = let (q :: BatchedDequeue Integer) = buildBatchedDequeue xs
                 in F.toList q == xs

prop_head_tail :: [Integer] -> Bool
prop_head_tail [] = True
prop_head_tail [x] = True
prop_head_tail (x:xs) = let (q :: BatchedDequeue Integer) = buildBatchedDequeue (x:xs)
                         in (Dequeue.head q == x) && (F.toList (Dequeue.tail q) == xs)



