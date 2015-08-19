{-# LANGUAGE RankNTypes #-}
{- |
Module      :  BatchedDequeue
Description :  ...
Copyright   :  (c) Sebastien Soudan
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module BatchedDequeue where

import qualified Data.Foldable as F
import           Dequeue

data BatchedDequeue a = BDQ [a] [a] deriving (Show)

instance F.Foldable BatchedDequeue where
     foldMap f q = if isEmpty q
                    then
                        mempty
                    else
                        f (Dequeue.head q) `mappend` F.foldMap f (Dequeue.tail q)

splitHalf :: forall a. [a] -> ([a], [a])
splitHalf l = splitAt ((length l) `div` 2) l

-- The idea is to maintain the invariant saying that both f and r are not empty when there is at least 2 elements
instance Dequeue BatchedDequeue where
    -- empty
    empty               = BDQ [] []
    -- isEmpty
    isEmpty (BDQ [] []) = True
    isEmpty _           = False

    -- cons
    cons x (BDQ [] [])  = BDQ [x] []            -- for the sake of the invariant
    cons x (BDQ [] [r]) = BDQ [x] [r]           -- for the sake of the invariant
    cons x (BDQ [f] []) = BDQ [x] [f]           -- for the sake of the invariant
    cons x (BDQ f r)    = BDQ (x:f) r           -- f and r are not empty
    -- head
    head (BDQ [] [])    = error "empty queue"
    head (BDQ (x:_) _)  = x
    head (BDQ [] r)     = Prelude.last r
    -- tail
    tail (BDQ [] [])    = error "empty queue"
    tail (BDQ [_] r)    = let (r1, r2) = splitHalf r    -- that the case were we want to split and
                           in BDQ (reverse r2) r1       -- reverse half of the other queue
    tail (BDQ (_:f) r)  = BDQ f r
    tail (BDQ [] r)     = let (r1, r2) = splitHalf r            -- that the case were we want to split and
                           in BDQ (drop 1 (reverse r2)) r1      -- reverse half of the other queue
    -- snoc
    snoc (BDQ [] []) x  = BDQ [] [x]            -- for the sake of the invariant
    snoc (BDQ [f] []) x = BDQ [f] [x]           -- for the sake of the invariant
    snoc (BDQ [] [r]) x = BDQ [r] [x]           -- for the sake of the invariant
    snoc (BDQ f r) x    = BDQ f (x:r)           -- f and r are not empty
    -- last
    last (BDQ [] [])    = error "empty queue"
    last (BDQ _ (r:_))  = r
    last (BDQ f [])     = Prelude.last f
    -- init
    init (BDQ [] [])    = error "empty queue"
    init (BDQ f [_])    = let (f1, f2) = splitHalf f
                           in BDQ f1 (drop 1 (reverse f2))
    init (BDQ f (_:r))  = BDQ f r
    -- size
    size (BDQ [] [])    = 0                     -- TODO fix this crappy implementation
    size (BDQ f r)      = length f + length r   -- TODO fix this crappy implementation


buildBatchedDequeue :: [a] -> BatchedDequeue a
buildBatchedDequeue = foldl snoc empty

