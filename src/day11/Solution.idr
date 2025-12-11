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

export
sol : String -> String
sol s = let graph := text s
            from := MkNode "you"
            to   := MkNode "out"
         in dfsSearch from to graph
         |> show

ex2 = """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""

export
sol2 : String -> String
-- sol2 _ = let graph := text ex2
sol2 s = let graph := text s
             from := MkNode "svr"
             stp1 := MkNode "dac"
             stp2 := MkNode "fft"
             endt := MkNode "out"
             n1  := dfsSearch stp1 stp2 graph
             n1' := dfsSearch stp2 stp1 graph
          in if (n1 == 0) -- the connection in the graph is from stp2 -> stp1
                then let n0 := dfsSearch from stp2 graph
                         n2 := dfsSearch stp1 endt graph
                      in show $ (n0 * n1' * n2)
                else let n0 := dfsSearch from stp1 graph
                         n2 := dfsSearch stp2 endt graph
                      in show $ (n0 * n1 * n2)
