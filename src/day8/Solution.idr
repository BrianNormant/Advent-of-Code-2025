module Solution

import Text.PrettyPrint.Bernardy

import Data.String
import Data.List1
import Data.List
import Data.SortedSet
import Data.Maybe
import Data.Fin

import Lib
import Debug.Trace

Point3D : Type
Point3D = (Integer, Integer, Integer)

distance : Point3D -> Point3D -> Double
distance (x1,y1,z1) (x2,y2,z2) =
  let x' := pow (cast (x2 - x1)) 2
      y' := pow (cast (y2 - y1)) 2
      z' := pow (cast (z2 - z1)) 2
   in sqrt $ x' + y' + z'


triple : List Integer -> Maybe Point3D
triple [x,y,z] = Just (x,y,z)
triple _ = Nothing

parseline : String -> Maybe Point3D
parseline l = case forget $ split (== ',') l of
                   [x,y,z] => traverse parseInteger [x,y,z]
                           |> (=<<) triple
                   _ => Nothing

--- for every point, list the other point in order of closeness
--- with this property
--- the first point only appear in the first list
--- the second in the the first and second, ect
closest : List Point3D -> List (List1 Point3D)
closest [] = []
closest [x] = [x:::[]]
closest (x::xs) = let clo := listClosest xs x
                   in (x:::clo)::(closest xs)
                   where
  listClosest : List Point3D -> Point3D -> List Point3D
  listClosest [] _ = []
  listClosest l@(_::_) n = sortBy (compare `on` (distance n)) l

EX4_a : List Point3D
EX4_a = [(0,0,1), (0,0,2), (0,0,5)]
EX4_b : List (List1 Point3D)
EX4_b = [
  (0,0,1):::[(0,0,2),(0,0,5)],
  (0,0,2):::[(0,0,5)],
  (0,0,5):::[]
  ]

closest_T1 : closest EX4_a = EX4_b
-- closest_T1 = Refl

--- we need to find the closest 2 node
findClosest : List (List1 Point3D) -> Maybe (Point3D, Point3D, List (List1 Point3D))
findClosest l = do s <- trySub (length l)
                   (a,b,repl,idx) <- go l Nothing s
                   idx' <- natToFin idx (length l)
                   let l' = replaceAt' l (complement idx') repl
                   pure $ (a,b,l') where
  go : List (List1 Point3D) -> Maybe (Point3D, Point3D, List1 Point3D, Nat) -> Nat ->
                Maybe (Point3D, Point3D, List1 Point3D, Nat)
  go [_] r Z = r
  go ((a:::b::c_a)::rest) Nothing (S n) = go rest (Just (a,b,a:::c_a,S n)) n
  go ((a:::b::c_a)::rest) (Just (a',b',r')) (S n) =
    if distance a b < distance a' b'
       then go rest (Just (a,b,a:::c_a,S n)) n
       else go rest (Just (a',b',r')) n
  go [] _ _ = Nothing
  go (_::_) _ _ = Nothing


--- because the number of repetion is different in example and input
checkRepeat : Nat -> Nat
checkRepeat 20 = 11
checkRepeat _ = 1000
updateGroups : List (SortedSet Point3D) -> Point3D -> Point3D -> List (SortedSet Point3D)
updateGroups gs a b = (case findIndices (\g => (contains a g || contains b g)) gs of
                           [i1,i2] => do i1' <- natToFin i1 $ length gs
                                         i2' <- natToFin i2 $ length gs
                                         let g2 = index' gs i2'
                                         let g1 = foldl (\g,p => insert p g) (index' gs i1') (index' gs i2')
                                         let g1 = insert b (insert a g1)
                                         let gs = replaceAt' gs i1' g1
                                         pure $ delete g2 gs
                           [i] => do i' <- natToFin i (length gs)
                                     let g = index' gs i'
                                     Just $ replaceAt' gs i' $ insert a (insert b g)
                           [] => Just $ (fromList [a,b]) :: gs
                           _ => Nothing
                      ) |> fromMaybe gs


iteration : List (List1 Point3D) -> List (SortedSet Point3D) -> (List (List1 Point3D), List (SortedSet Point3D))
iteration close groups =
  case findClosest close of
       Just (a,b,c) => (c, updateGroups groups a b)
       Nothing => (close, groups)

||| find the closests node, remove them and add them to the linked nodes
linkClosests : List (List1 Point3D) -> (List (List1 Point3D), List (SortedSet Point3D))
linkClosests i = repeat 1000 (\(c,g) => iteration c g) (i,[])


export
sol : String -> String
sol l = lines l
     |> traverse parseline
     |> maybe "error parsing" (
       closest
       ||> linkClosests
       ||> snd
       ||> map toList
       ||> map (natToInteger . length)
       ||> sortBy (\a,b => b `compare` a)
       ||> take 3
       ||> (\c => case c of
                       [l1,l2,l3] => l1 * l2 * l3
                       _ => 0
           )
       ||> pretty
       ||> Doc.render (Opts 60)
       -- ||> show
       )


||| find the closests node, remove them and add them to the linked nodes
linkClosests2 : List (List1 Point3D) -> (List (List1 Point3D), List (SortedSet Point3D))
linkClosests2 i = let s := map (singleton . head) i
                   in until' 99999 (\(_,g) => length g == 2) (\(c,g) => iteration c g) (i,s)

|||
findClosest2 : List (SortedSet Point3D) -> Maybe (Point3D, Point3D)
findClosest2 [g1, g2] = let g1 := Prelude.toList g1
                            g2 := Prelude.toList g2
                            z := bisequence (g1,g2)
                              |> sortBy (\(a1,a2),(b1,b2) => compare (distance a1 a2)
                                                                     (distance b1 b2))
                              |> head'
                         in z
findClosest2 _ = Nothing

export
sol2 : String -> String
sol2 l = lines l
      |> traverse parseline
      |> maybe "error parsing" (
        closest
        ||> linkClosests2
        ||> snd
        ||> findClosest2
        ||> map (\((x1,_),(x2,_)) => x1 * x2)
        ||> fromMaybe 0
        ||> pretty
        ||> Doc.render (Opts 60)
        -- ||> show
        )
