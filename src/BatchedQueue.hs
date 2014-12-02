{- |
Module      :  BatchedQueue
Description :  ...
Copyright   :  (c) Sebastien Soudan
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module BatchedQueue where

import           Queue

data BatchedQueue a = Q [a] [a] deriving (Show)

-- | BatchedQueue as an example of FIFO queue
--
-- [Purely functional Data Structures, Okasaki, p.92]
-- Queue is implemented a pair of lists.
--
-- (f,r): f contains the front elements and r the rear elements in reverse order
--
-- Using this representation, head is the first element of f and that the element
-- tail must remove.
--
-- The last element is the first of r and since r is in reverse order
-- snoc need to put the new element at the head of r
--
-- We want to maintain the invariant f is empty only if r is also empty
-- so that we won't end up with a case where f being empty we have to find the head at the tail of r in O(n)
-- This way, head is always O(1).
--
-- Method   Amortized  Worst
-- head     O(1)       O(1)
-- tail     O(1)       O(n)
-- snoc     O(1)       O(1)
-- isEmpty  O(1)       O(1)
-- size     O(n)!!     O(n)!! [TODO need to change size by storing th size]
--
instance Queue BatchedQueue where
    -- empty
    empty               = (Q [] [])
    -- isEmpty
    isEmpty (Q [] _)    = True
    isEmpty _           = False
    -- snoc
    snoc (Q [] _) x     = (Q [x] [])    -- empty f => empty r
    snoc (Q f r) x      = (Q f (x:r))   -- f is not empty
    -- head
    head (Q [] _)       = error "empty queue"
    head (Q (x:_) _)    = x
    -- tail
    tail (Q [] _)       = error "empty queue"
    tail (Q (_:[]) r)   = Q (reverse r) []
    tail (Q (_:f) r)    = Q f r
    -- size
    size (Q [] _)       = 0
    size (Q f r)        = length f + length r -- TODO costly implementation

buildBatchedQueue :: [a] -> BatchedQueue a
buildBatchedQueue l = foldl snoc empty l

