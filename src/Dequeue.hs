{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
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

-- import qualified Data.Foldable as F
-- import           Data.Monoid

-- Double-ended queue:
-- [Purely functional Data Structures, Okasaki, p.44]
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

-- instance Dequeue a => F.Foldable a where
--      foldMap f q = if isEmpty q
--                     then
--                         mempty
--                     else
--                         f (Dequeue.head q) `mappend` F.foldMap f (Dequeue.tail q)

instance Dequeue a => Functor a where
     fmap f q = if isEmpty q 
                    then 
                        empty
                    else
                        snoc (fmap f (Dequeue.tail q)) (f (Dequeue.head q))

