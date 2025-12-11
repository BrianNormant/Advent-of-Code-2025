module Solution

import Data.String
import Data.List
import Data.SortedMap

import Lib
import Debug.Trace

data Node : Type where
  MkNode : String -> Node

Eq Node where
  (==) (MkNode a) (MkNode b) = a == b

Ord Node where
  compare (MkNode a) (MkNode b) = compare a b

Show Node where
  show (MkNode a) = show a

parseLine : String -> (Node, List Node)
parseLine s = let (from, to) := break (== ':') s
                  (_, to) := span (== ':') to
                  to := words to
               in (MkNode from, map MkNode to)

text : String -> SortedMap Node (List Node)
text str  = str
         |> lines
         |> map parseLine
         |> foldl (\m,(f,t) => update (\o =>
                   case o of
                        Nothing => Just $ t
                        Just o' => Just $ o' ++ t
                  ) f m ) empty

Graph : Type
Graph = SortedMap Node (List Node)

Cache : Type
Cache = SortedMap Node Nat

||| search the number of path from the
||| first arg to the last arg
||| use depth first with memo
dfsSearch : (f: Node) -> (t: Node) -> Graph -> Nat
dfsSearch a1 a2 a3 = fst $ dfs empty a1 a2 a3
  where
    dfs : Cache -> Node -> Node -> Graph -> (Nat, Cache)
    searchNext : Node -> Graph -> (Nat, Cache) -> Node -> (Nat, Cache)
    searchNext t g (s, c) n with (lookup n c)
      _ | Just i = (s + i, c)
      _ | Nothing = let (i, c) := dfs c n t g
                     in (s + i, insert n i c)
    dfs cache from to graph with (from == to, lookup from graph)
      _ | (True, _) = (1, cache)
      _ | (False, Nothing) = (0, cache)
      _ | (False, Just next) = foldl (searchNext to graph) (0, cache) next

strex : String
strex = """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
"""

export
sol : String -> String
sol s = let graph := text s
            from := MkNode "you"
            to   := MkNode "out"
         in dfsSearch from to graph
         |> show

export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
