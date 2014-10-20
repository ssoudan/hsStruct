{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Dequeue
Description :  ... 
Copyright   :  (c) Sebastien Soudan 
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Dequeue where

-- Double-ended queue:
-- [Okasaki, p. ??]
--
-- ML signature:
-- signature DEQUE = 
-- sig
--     type alpha Queue
--     exception EMPTY

--     val empty : ￼ Queue
--     val isEmpty : ￼ Queue -> bool
--     -- (insert, inspect, and remove the front element)
--     val cons: alpha x alpha Queue -> alpha Queue 
--     val head: alpha Queue -> alpha          -- raises EMPTY if queue is empty
--     val tail: alpha Queue -> alpha Queue    -- raises EMPTY if queue is empty
--  ￼
--     -- (insert, inspect, and remove the rear element)￼￼￼￼￼￼￼
--     val snoc: alpha Queue x alpha -> alpha Queue
--     val last: alpha Queue -> alpha
--     val init: alpha Queue -> alpha Queue

-- end
--

class Dequeue dq where 
    empty :: dq a
    isEmpty :: dq a -> Bool

    cons :: a -> dq a -> dq a -- append to the head
    head :: dq a -> a
    tail :: dq a -> dq a

    snoc :: dq a -> a -> dq a -- append to the tail
    last :: dq a -> a
    init :: dq a -> dq a

    size :: dq a -> Int


data BatchedDequeue a = BDQ [a] [a] deriving (Show)

splitHalf :: forall a. [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

-- The idea is to maintain the invariant saying that both f and r are not empty when there is at least 2 elements
instance Dequeue BatchedDequeue where
    empty = (BDQ [] [])
    isEmpty (BDQ [] []) = True
    isEmpty _ = False

    cons x (BDQ [] r) = BDQ [x] r       -- also account for '[] []' case
    cons x (BDQ [f] []) = BDQ [x] [f]   -- for the sake of the invariant 
    cons x (BDQ f r) = BDQ (x:f) r      -- f and r are not empty
    head (BDQ [] []) = error "empty queue" 
    head (BDQ (x:_) _) = x
    --head (BDQ [] r) = ..              -- not supposed to happen
    tail (BDQ [] []) = error "empty queue"
    tail (BDQ [_] r) = let (r1, r2) = splitHalf r   -- that the case were we want to split and 
                        in (BDQ (reverse r2) r1)    -- reverse half of the other queue
    tail (BDQ (_:f) r) = BDQ f r 

    snoc (BDQ f []) x = BDQ f [x]       -- also account for '[] []' case
    snoc (BDQ [] [r]) x = BDQ [x] [r]   -- for the sake of the invariant
    snoc (BDQ f r) x = BDQ f (x:r)      -- f and r are not empty
    last (BDQ [] []) = error "empty queue"
    last (BDQ _ (r:_)) = r
    --last (BDQ _ []) = ..              -- not supposed to happen
    init (BDQ [] []) = error "empty queue"
    init (BDQ f [_]) = let (f1, f2) = splitHalf f
                        in (BDQ f1 (reverse f2))
    init (BDQ f (_:r)) = (BDQ f r)

    size (BDQ [] []) = 0                    -- TODO fix this crapy implementation
    size (BDQ f r) = length f + length r    -- TODO fix this crapy implementation




