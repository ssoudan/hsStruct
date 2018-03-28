{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{- |
Module      :  RDG
Description :  ...
Copyright   :  (c) Sebastien Soudan
License     :  APLv2

Maintainer  :  sebastien.soudan@gmail.com
Stability   :  experimental
Portability :  portable

-}

module RDG where

import           Data.Digest.Murmur32
import           Data.Foldable        as F
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Control.Arrow
import           Graph


type PartitionID = ID



----------------------------------------
type EdgeTablePartition e = [Edge e]

type EdgeTable e = Map.Map PartitionID (EdgeTablePartition e)

----------------------------------------
type VertexDataTablePartition v = [Vertex v]

newtype VertexDataTable v = VertexDataTable (Map.Map PartitionID (VertexDataTablePartition v)) deriving (Show)

type VertexMapPartition = [(VertexID, PartitionID)]

type VertexMap = Map.Map PartitionID VertexMapPartition

----------------------------------------

data RDG v e = RDG { maxId           :: ID
                   , edgeTable       :: EdgeTable e
                   , vertexDataTable :: VertexDataTable v
                   , vertexMap       :: VertexMap
                   } deriving (Show)

----------

partitions :: Integer
partitions = 16

isqrt :: Integer -> Integer
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

hashVertex :: VertexID -> PartitionID
hashVertex v = fromIntegral $ asWord32 $ hash32 v

hashEdge :: forall e. Edge e -> PartitionID
hashEdge (Edge s d _) = let sqrtM = isqrt partitions
                            hS = hashVertex s
                            hD = hashVertex d
                         in sqrtM * (hS `mod` sqrtM) + hD `mod` sqrtM
----------

insertEdgeInEdgeTable :: forall e. EdgeTable e -> Edge e -> EdgeTable e
insertEdgeInEdgeTable etp e = Map.insertWith (++) (hashEdge e) [e] etp
----------

insertVertexInVertexDataTable :: forall v. VertexDataTable v -> Vertex v -> VertexDataTable v
insertVertexInVertexDataTable (VertexDataTable vdt) vv = VertexDataTable (Map.insertWith (++) (hashVertex $ vertexId vv) [vv] vdt)

----------

insertEdgeInVertexMap :: forall e. VertexMap -> Edge e -> VertexMap
insertEdgeInVertexMap v e@(Edge s d _) = let pId = hashEdge e
                                             sHash = hashVertex s
                                             dHash = hashVertex d
                                          in Map.insertWith (++) sHash [(s,pId)] $ Map.insertWith (++) dHash [(d,pId)] $ v

----------

emptyRDG :: forall v e. RDG v e
emptyRDG = RDG 0 Map.empty (VertexDataTable Map.empty) Map.empty

isEmptyRDG :: forall v e. RDG v e -> Bool
isEmptyRDG (RDG _ e (VertexDataTable v) m) = Map.null v && (Map.null e) && (Map.null m)

nextID :: forall v e. RDG v e -> ID
nextID rdg = (maxId rdg) + 1

----------

insertVertex :: forall v e. RDG v e -> v -> (RDG v e, VertexID)
insertVertex r d = let vId = nextID r
                       v = Vertex vId d
                       vdtp = insertVertexInVertexDataTable (vertexDataTable r) v
                       nR = RDG vId (edgeTable r) vdtp (vertexMap r)
                    in (nR, vId)


insertEdge :: forall v e. RDG v e -> Edge e -> RDG v e
insertEdge r e = let eId = maxId r
                     eT = insertEdgeInEdgeTable (edgeTable r) e
                     vdTP = vertexDataTable r
                     vM = insertEdgeInVertexMap (vertexMap r) e
                  in RDG eId eT vdTP vM

-- TODO check how to implement this with mapOnAllVertices
mapV :: forall v w e. ((Vertex v) -> w) -> RDG v e -> RDG w e
mapV f g =  let VertexDataTable v = vertexDataTable g
             in RDG (maxId g) (edgeTable g) (VertexDataTable (Map.map (Prelude.map (Graph.mapV f)) v)) (vertexMap g)

-- TODO use Dlist
buildFromList :: forall v e. [v] -> (RDG v e, [VertexID])
buildFromList = Prelude.foldl appendVertices (emptyRDG, [])
    where
      appendVertices (g,vs) v = let (nG, vId) = insertVertex g v
                                 in (nG, vs ++ [vId])


addEdges :: forall v e. Eq v => [v] -> [(v, v, e)] -> (RDG v e, [Maybe (Edge e)])
addEdges nodes edges =
  let (g, ids) = buildFromList nodes
      m = zip nodes ids
      look (s, d, da) = case (lookup s m, lookup d m) of
                          (Just ss, Just dd) -> Just (Edge ss dd da)
                          (_, _) -> Nothing
  in ( g
     , filter isJust $ Prelude.map look edges)

-- | Build a graph from a list of vertex data (used as vertex name) and a list of edges
-- defined by the vertex name of the source and destination and the edge name.
--
-- Note this uses the constant 'partitions' which defines the total number of partitions
-- to split the data across. It has to be a power of 2 and is 16 by default.
--
-- Example with 16 partitions:
-- > buildGraph ["A", "B", "C"] [("A", "B", "AB"), ("B", "C", "BC")]
-- RDG {maxId = 3,
-- edgeTable = fromList [
--  (3,[Edge {sourceId = 2, destinationId = 3, edgeData = "BC"}]),
--  (8,[Edge {sourceId = 1, destinationId = 2, edgeData = "AB"}])],
-- vertexDataTable = fromList [
--  (193032412, [Vertex {vertexId = 2, vertexData = "B"}]),
--  (1500603050,[Vertex {vertexId = 1, vertexData = "A"}]),
--  (2798830455,[Vertex {vertexId = 3, vertexData = "C"}])],
-- vertexMap = fromList [
--  (193032412, [(2,3),(2,8)]),
--  (1500603050,[(1,8)]),
--  (2798830455,[(3,3)])]}
buildGraph :: forall v e. Eq v => [v] -> [(v, v, e)] -> RDG v e
buildGraph nodes edges = let (g, resolvedEdges) = addEdges nodes edges
                          in Prelude.foldl (\ r e -> case e of Nothing -> r
                                                               Just ee -> insertEdge r ee) g resolvedEdges
edges :: forall v e. RDG v e -> [Edge e]
edges rdg = let es = Map.toList $ edgeTable rdg
             in concatMap snd es

vertices :: forall v e. RDG v e -> [Vertex v]
vertices g = let VertexDataTable v = vertexDataTable g
              in concatMap snd $ Map.toList v

-- | apply message generating function (f) to a VertexDataTable (r)
-- TODO: each partition could be processed concurrently (different thread?)
instance RDD (VertexDataTable v) ID (Vertex v) where
  map :: forall v m. Message m => (VertexDataTable v) -> (Vertex v -> (ID, m)) -> [(ID, m)]
  map (VertexDataTable r) f = let vertices = concatMap snd $ Map.toList r
                               in Prelude.map f vertices

---------------------------------------------------------------------------------

type InitialMsg v m = (Vertex v) -> m
type VProgF v m = (Vertex v, m) -> v
type SendMsgF v e m = Vertex v -> Edge e -> Vertex v -> Maybe m
type CombineF m = (m, m) -> m


class Pregel g v e where
  compute :: forall m. g v e-> InitialMsg v m -> VProgF v m -> SendMsgF v e m -> CombineF m -> g v e

-- | apply a f on all the vertices of a graph
--
-- > let g = buildGraph ["A", "B", "C"] [("A", "B", "AB"), ("B", "C", "BC")]
-- > mapOnAllVertices g (\(Vertex id v) -> Vertex id (v ++ "Z"))
-- [(2, Vertex {vertexId = 2, vertexData = "BZ"}), (1, Vertex {vertexId = 1, vertexData = "AZ"}), (3, Vertex {vertexId = 3, vertexData = "CZ"})]
mapOnAllVertices :: forall v e a. RDG v e -> (Vertex v -> a) -> [(VertexID, a)]
mapOnAllVertices graph f = Prelude.map (\ v@(Vertex vid _) -> (vid, (f v))) vs
        where vs = vertices graph

applyVprogF :: forall v m. [(VertexID, m)] -> VProgF v m -> VertexDataTable v -> VertexDataTable v
applyVprogF msgs vprogf (VertexDataTable vdt) =
  let oldVertices = F.foldMap id vdt
      -- newVertices :: forall v. [Vertex v]
      newVertices = Prelude.map doUpdate oldVertices
      -- doUpdate :: forall v. Vertex v -> Vertex v
      doUpdate v@(Vertex vid _) =
        case lookup vid msgs of
          Just message -> Vertex vid (vprogf (v, message))
          Nothing -> v -- no message for this vertex
  in Prelude.foldl insertVertexInVertexDataTable (VertexDataTable Map.empty) newVertices


-- | updateGraph replaces vertex in a graph
updateGraph :: forall m v e. [(VertexID, m)] -> RDG v e -> VProgF v m -> RDG v e
updateGraph msgs graph vprogf =
  RDG
  { maxId = maxId graph
  , edgeTable = edgeTable graph
  , vertexDataTable = applyVprogF msgs vprogf (vertexDataTable graph)
  , vertexMap = vertexMap graph
  }

sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

-- TODO: look at https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-List.html#g:7
onMsgs :: forall m . CombineF m -> (VertexID, [Maybe m]) -> Maybe (VertexID, m)
onMsgs combinef (k,v) =
  let l = Data.Maybe.catMaybes v
   in case l of [] -> Nothing
                otherwise -> Just (k, Prelude.foldl1 (curry combinef) l)

aggregateNeighbors :: forall v e m . RDG v e -> SendMsgF v e m -> CombineF m -> [(VertexID, m)]
aggregateNeighbors graph sendMsgf combinef =
  let e = edges graph
      vs = Prelude.map (\v -> (vertexId v, v)) $ vertices graph
      send e = sendMsgf (fromJust s) e (fromJust d) -- TODO: not too pretty
               where s = lookup (sourceId e) vs
                     d = lookup (destinationId e) vs
      -- msgs :: [(VertexID, Maybe m)]
      msgs = Prelude.map (destinationId &&& send) e -- we send the message to the destination vertex
      -- groupedMsgs :: Map.Map VertexID [Maybe m]
      groupedMsgs = sortAndGroup msgs
   in Data.Maybe.catMaybes $
    Prelude.map
        (onMsgs combinef)
        (Map.toList groupedMsgs)

-- TODO
-- [√] need a function to iterate over the vertices
-- [√] need a function to update a graph
-- [ ] need to actually send messages, don't we? - not sure where... need to read the paper again
--     [√] sort of emulated that part
-- [√] need a function to aggregate messages
-- [√] implement some algorithms from https://amplab.cs.berkeley.edu/wp-content/uploads/2014/09/graphx.pdf
-- [ ] more tests
-- [ ] split the processing on the partitions
-- [ ] cleanup


-- | compute_ runs a computation on a graph
-- Code from spark implementation:
--  def Pregel(graph:       Graph[V,E],
--             initialMsg:  M
--             vprogf:      ((Id,V), M) => V,
--             sendMsgf:    Edge[V,E] => Option[M],
--             combinef:    (M,M) => M,
--             numIter:     Long): Graph[V,E] = {
--    // Initialize the messages to all vertices
--    var msgs: RDD[(Vid, A)] = graph.vertices.map(v => (v.id, initialMsg))
--    // Loop while their are messages
--    var i = 0
--    while (msgs.count > 0 && i < maxIter) {
--      // Receive the message sums on each vertex
--      graph = graph.updateVertices(msgs, vprogf)
--      // Compute and combine new messages
--      msgs = graph.aggregateNeighbors(sendMsgf, combinef)
--      i = i+1
--    }
--  }
compute_ :: forall m v e. RDG v e -> InitialMsg v m -> VProgF v m -> SendMsgF v e m -> CombineF m -> RDG v e
compute_ graph initialMsg vprogf sendMsgf combinef =
  let msgs = mapOnAllVertices graph initialMsg
   in doCompute msgs graph
  where
    doCompute m g =
      let newGraph = updateGraph m g vprogf
          newMsgs = aggregateNeighbors newGraph sendMsgf combinef
      in if null newMsgs
           then newGraph
           else doCompute newMsgs newGraph

instance Pregel RDG v e where
  compute = compute_

-- | connectedComp computes the connected components of a graph
-- by returning a graph where vertices are labelled with their class
connectedComp :: forall v e. RDG v e -> RDG VertexID e
connectedComp g = let g' = RDG.mapV (\v -> vertexId v) g
                      initialMsg :: InitialMsg v VertexID
                      initialMsg = (\v -> vertexId v)
                      vprogF :: (Vertex VertexID, VertexID) -> VertexID
                      vprogF = (\ (v, m) -> min (vertexData v) m)
                      sendMsgF :: Vertex VertexID -> Edge e -> Vertex VertexID -> Maybe VertexID
                      sendMsgF s _ d = if (vertexData s < vertexData d) then
                                      Just $ vertexData s
                                    else
                                      Nothing
                      combineF :: (VertexID, VertexID) -> VertexID
                      combineF = uncurry min
                  in compute g' initialMsg vprogF sendMsgF combineF

-- From Pregel paper:
-- def ConnectedComp(g: Graph[V, E]) = {
--   g = g.mapV(v => v.id) // Initialize vertices
--   def vProg(v: Id, m: Id): Id = {
--     if (v == m) voteToHalt(v)
--       return min(v, m)
--   }
--   def sendMsg(t: Triplet): Id =
--     if (t.src.cc < t.dst.cc)
--         t.src.cc
--       else
--         None // No message required
--   def gatherMsg(a: Id, b: Id): Id = min(a, b)
--
--   return Pregel(g, vProg, sendMsg, gatherMsg)
-- }
