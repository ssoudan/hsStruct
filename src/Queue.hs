{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{- |
Module      :  Queue
Description :  ...
Copyright   :  (c) Sebastien Soudan
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Queue where

import qualified Data.Foldable as F
import           Data.Monoid

-- | A FIFO queue
class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: q a -> a -> q a
    head :: q a -> a
    tail :: q a -> q a

instance Queue a => F.Foldable a where
     foldMap f q = if isEmpty q
                    then
                        mempty
                    else
                        f (Queue.head q) `mappend` F.foldMap f (Queue.tail q)

instance Queue a => Functor a where
     fmap f q = if isEmpty q 
                    then 
                        empty
                    else
                        snoc (fmap f (Queue.tail q)) (f (Queue.head q))      

-- 
-- toList :: (Queue q, F.Foldable q) => q a -> [a]
-- toList qq = F.foldMap (\x ->[x]) qq
--
-- or
-- toList u =
    -- | Queue.isEmpty u = []
    -- | otherwise     = Queue.head u:pullAll (Queue.tail u)
--
-- But this is already available via Data.Foldable.toList