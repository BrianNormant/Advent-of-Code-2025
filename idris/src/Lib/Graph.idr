module Lib.Graph

import Data.Maybe
import Data.SortedMap

import Lib.Limits

private
Key : Type
Key = Bits64

private
Weight : Type
Weight = Bits64


||| One node in the graph
||| identified by its id
record Node ty where
  constructor MkNode
  el : ty
  id : Key

||| A directed edge in the graph
||| between two nodes
record Edge ty where
  constructor MkEdge
  from : Node ty
  to : Node ty
  weight : Weight

||| Internal representation of the graph
BitMap : Type
BitMap = SortedMap Key (SortedMap Key Weight)

||| Interface of a graph
record Graph ty where
  constructor MkGraph
  size : Key
  internal : BitMap
  elements : SortedMap Key ty

export
newGraph : Key -> Graph ty
newGraph size = MkGraph size empty empty

export
addNode : Graph ty -> ty -> Graph ty
addNode (MkGraph size bm els) el =
  MkGraph (size + 1) bm (insert (size + 1) el els)

export
addEdge : Weight -> Key -> Key -> Graph ty -> Maybe (Graph ty)
addEdge weight from to graph with (compare from graph.size, compare to graph.size)
  _ | (LT, LT) = Just $ { internal $= \bm => update (add weight) from bm } graph
    where
      add : Weight -> Maybe (SortedMap Key Weight) -> Maybe (SortedMap Key Weight)
      add weight Nothing = Just $ insert to weight empty
      add weight (Just bm) = Just $ insert to weight bm
  _ | _ = Nothing

export
getNodes : Graph ty -> List (Node ty)
getNodes (MkGraph _ _ els) = SortedMap.toList els
                          |> map swap
                          |> map (uncurry MkNode)

export
||| find the shortest path using the dijkstra algorithm
dijkstra : Graph ty -> Key -> Key -> List (Node ty)
dijkstra graph from to = ?intfn where
  ||| update the dist and pred where the new minimun distance to reach
  dijkstra' : (curr : Node ty)
          -> (orig : Node ty)
          -> (goal : Node ty)
          -> (weight : Weight)
          -> (dist : SortedMap Key Weight)
          -> (visit : SortedMap Key Bool)
          -> (pred : SortedMap Key Key)
          -> (adj : BitMap)
          -> (graph : Graph ty)
          -> List (Node ty)
  dijkstra' curr orig goal w dist visit pred adj graph =
    -- mark this node as visited
    let visit = insert curr.id True visit
    -- update the distmap with the adjacent nodes
        nei = maybe [] SortedMap.toList $ lookup curr.id adj
        nei = filter (\(k,_) => maybe True not $ lookup k visit ) nei -- don't visit already visited
        nei = filter (\(_,v) => v /= 0) nei
        dist = foldl (\d,(k,v) => update (\b =>
               case b of
                    Just b' => Just $ min b' (v+w)
                    Nothing => Just $ v+w
          ) k d) dist nei
        pred = foldl (
          \p,(k,w') => let mw = lookup k dist in
                          case mw of
                               Just mw' => if mw' == ()
          ) pred nei
        -- Get the next minimun
        (nk, nv) = foldl (\(k',v'),(k,v) => if v < v' then (v, k) else (v', k') ) (0, BITS64_MAX) (SortedMap.toList dist)
        el = lookup nk graph.elements
     in case el of
             Just el => dijkstra' (MkNode el nk) orig goal (w + nv) dist visit pred adj graph
             Nothing => [] -- impossible
