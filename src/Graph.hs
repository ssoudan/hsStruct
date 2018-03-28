{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{- |
Module      :  Graph
Description :  ...
Copyright   :  (c) Sebastien Soudan
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Graph where

import Data.Functor

-- from [GraphX: A resilient distributed graph system on Spark, 2013]
--
-- class Graph[V, E] {
-- def vertices(): RDD[(Id, V)]
-- def edges(): RDD[(Id, Id, E)]
-- def filterVertices(f: (Id, V)=>Bool): Graph[V, E]
-- def filterEdges(f: Edge[V, E]=>Bool): Graph[V, E]
-- def mapVertices(f: (Id,V)=>(Id,V2)): Graph[V2, E]
-- def mapEdges(f: (Id, Id, E)=>(Id, Id, E2)): Graph[V, E2]
-- def updateVertices(tbl: RDD[(Id, A)],
--  func: (Id, V, A)=>(Id, V2)): Graph[V2, E]
-- def aggregateNeighbors(
--  mapFunc: (Id, Edge[V, E]) => A,
--  reduceFunc: (A, A) => A): RDD[(Id, A)]
-- def reverseEdgeDirection(): Graph[V, E] = mapEdges(e => (e.dst, e.src, e.data))
-- def degree(): RDD[(Id, Int)] = aggregateNeighbors((id, e) => 1, (a, b)=> a + b)
-- }
--

class Message m where


class RDD r i v where
    map :: (Message m) => r -> ( v -> (i, m) ) -> [(i, m)]


type ID = Integer

type VertexID = ID

--type EdgeData = String

data Edge e = Edge { sourceId      :: VertexID
                   , destinationId :: VertexID
                   , edgeData      :: e
                   } deriving (Show)
--
--type VertexData = String

data Vertex v = Vertex { vertexId   :: VertexID
                       , vertexData :: v
                       } deriving (Show)

instance Functor Vertex where
  fmap f (Vertex i d) = Vertex i (f d)

mapV :: forall v w. (Vertex v -> w) -> Vertex v -> Vertex w
mapV f v@(Vertex i d) = Vertex i (f v)


--------------------------------

--class Graph g where
    --vertices :: forall r. (F.Foldable r) => g -> r g
    --vertices :: (RDD r ID Vertex) => g -> r
    --edges :: g -> RDD [(Id, Id, e)]
    --filterVertives ::  g -> ((VertexID, v) -> Bool) -> g
    --filterEdges ::  g -> (Edge -> Bool) -> g
    --mapVertices :: g -> ((VertexID, v) -> (VertexID, v2)) -> g v2 e
    --mapEdges :: g -> ((VertexID, VertexID, e) -> (VertexID, VertexID, e2)) -> g2
    --updateVertices
    --aggregateNeighbors :: forall r. (RDD r VertexID Vertex) => forall a. g -> ((VertexID, Edge) -> a) -> (a -> a -> a) -> r
