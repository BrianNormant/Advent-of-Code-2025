module Solution

import Data.String
import Data.List
import Data.List1
import Data.List.Lazy
import Data.Maybe

import Lib
import Debug.Trace

parseLine : String -> Maybe (Integer, Integer)
parseLine = split (== ',')
        ||> forget
        ||> (\c => case c of
                        [n1,n2] => do n1 <- parseInteger n1
                                      n2 <- parseInteger n2
                                      Just (n1,n2)
                        _ => Nothing
            )

area : (Integer, Integer) -> (Integer, Integer) -> Integer
area (ax,ay) (bx,by) =
  let x' := abs $ bx - ax
      y' := abs $ by - ay
   in (x' + 1) * (y' + 1)

Segment : Type
Segment = ((Integer, Integer), (Integer, Integer))

det4 : Integer -> Integer -> Integer -> Integer -> Double
det4 a b c d = cast $ a*d - c*b


doubleEq : Double -> Double -> Bool
doubleEq a b = let d := abs (a - b)
                in d < 0.001

||| test if 2 segment intersect
intersection : Segment -> Segment -> Bool
intersection ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  if ( (x1 == x2 && y3 == y4)
     || (y1 == y2 && x3 == x4) )
     && (not (x1 == x2 && x2 == x3 && x3 == x4))
     && (not (y1 == y2 && y2 == y3 && y3 == y4))
     then let t := (det4 (x1-x3) (x3-x4) (y1-y3) (y3-y4))
                 / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
              u := -(det4 (x1-x2) (x1-x3) (y1-y2) (y1-y3))
                 / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
              c := case (doubleEq t 0, doubleEq u 0, doubleEq t 1, doubleEq u 1) of
                        (True, True, _) => False
                        (True, False, _, True) => False
                        (_, True, True, _) => False
                        (_, _, True, True) => False
                        _ => True
           in 0 <= t && t <= 1 && 0 <= u && u <= 1
              && c
     else False

intersection_T1 : intersection ((2,1),(2,3)) ((1,2),(3,2)) = True
intersection_T1 = Refl
intersection_T2 : intersection ((3,4),(3,7)) ((5,5),(5,9)) = False
intersection_T2 = Refl
intersection_T3 : intersection ((1,1),(1,5)) ((1,1),(1,5)) = False
intersection_T3 = Refl
intersection_T4 : intersection ((1,6),(1,4)) ((1,4),(2,4)) = True
-- intersection_T4 = Refl
intersection_T5 : intersection ((1,6),(1,5)) ((1,4),(2,4)) = False
intersection_T5 = Refl

segments : List (Integer, Integer) -> List Segment
segments l = (do he <- head' l
                 la <- last' l
                 sl <- slidingWindows 2 l
                    |> toList
                    |> traverse pairFromList
                 pure $ (he,la) :: sl
             ) |> fromMaybe []

export
sol : String -> String
sol = lines
  ||> traverse parseLine
  ||> map (
    pair
    ||> bisequence
    ||> filter (uncurry (/=))
    ||> sortBy (compare `on` (uncurry area))
    )
  ||> (=<<) last'
  ||> maybe 0 (uncurry area)
  ||> show

||| edge intersection test
countIntersection : List Segment -> Segment -> Nat
countIntersection l s = filter (intersection s) l
                     |> length


intersectionNC : Segment -> Segment -> Bool
intersectionNC ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  case ((x1 == x2 && x2 == x3 && x3 == x4),
        (y1 == y2 && y2 == y3 && y3 == y4)) of
       (True, True) => True
       (True, False) => (y1 > y3 && y3 > y2) ||
                        (y2 > y3 && y3 > y1) ||
                        (y1 > y4 && y4 > y2) ||
                        (y2 > y4 && y4 > y1)
       (False, True) => (x1 > x3 && x3 > x2) ||
                        (x2 > x3 && x3 > x1) ||
                        (x1 > x4 && x4 > x2) ||
                        (x2 > x4 && x4 > x1)
       (False, False) => let t := (det4 (x1-x3) (x3-x4) (y1-y3) (y3-y4))
                                / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
                             u := -(det4 (x1-x2) (x1-x3) (y1-y2) (y1-y3))
                                / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
                          in 0 <= t && t <= 1 && 0 <= u && u <= 1

||| true if 2 segment are intersecting
||| in the parallel sense, ex:
||| 0---0-----0----0
throughSegment : Segment -> Segment -> Bool
throughSegment ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  case ((x1 == x2 && x2 == x3 && x3 == x4),
        (y1 == y2 && y2 == y3 && y3 == y4)) of
       (True, True) => True
       (True, False) => (y1 > y3 && y3 > y2) ||
                        (y2 > y3 && y3 > y1) ||
                        (y1 > y4 && y4 > y2) ||
                        (y2 > y4 && y4 > y1)
       (False, True) => (x1 > x3 && x3 > x2) ||
                        (x2 > x3 && x3 > x1) ||
                        (x1 > x4 && x4 > x2) ||
                        (x2 > x4 && x4 > x1)
       (False, False) => False

intersectionND : Segment -> Segment -> Bool
intersectionND ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  case ((x1 == x2 && x2 == x3 && x3 == x4),
        (y1 == y2 && y2 == y3 && y3 == y4)) of
       (True, True) => True
       (True, False) => False
       (False, True) => False
       (False, False) => let t := (det4 (x1-x3) (x3-x4) (y1-y3) (y3-y4))
                                / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
                             u := -(det4 (x1-x2) (x1-x3) (y1-y2) (y1-y3))
                                / (det4 (x1-x2) (x3-x4) (y1-y2) (y3-y4))
                          in 0 < t && t < 1 && 0 < u && u < 1

||| point inside test
||| we just cast a ray toward right
||| if count is odd, inside
||| if count is even, outside
||| max value in the poly is ~100000
pointInside : List Segment -> (Integer, Integer) -> Bool
pointInside seg pt =
  let onSurface := any (
        \(p1,p2) => case ((fst p1 == fst p2 && fst p2 == fst pt),
                          (snd p1 == snd p2 && snd p2 == snd pt)) of
                         (True, True) => True
                         (True, False) => ((snd p1) >= (snd pt) && (snd pt) >= (snd p2)) ||
                                          ((snd p2) >= (snd pt) && (snd pt) >= (snd p1))
                         (False, True) => ((fst p1) >= (fst pt) && (fst pt) >= (fst p2)) ||
                                          ((fst p2) >= (fst pt) && (fst pt) >= (fst p1))
                         (False, False) => False
        ) seg
   in onSurface || towards seg pt
      where
        towardsL : List Segment -> (Integer, Integer) -> Bool
        towardsR : List Segment -> (Integer, Integer) -> Bool
        towardsU : List Segment -> (Integer, Integer) -> Bool
        towardsD : List Segment -> (Integer, Integer) -> Bool
        towardsL seg pt = let pt' := (fst pt, 200000)
                              -- true intersection
                              in1 := filter (intersectionND (pt,pt')) seg
                                  |> length |> natToInteger
                              -- false intersection (going through line)
                              in2 := filter (throughSegment (pt,pt')) seg
                                  |> length |> natToInteger
                           in if in1 == 0
                                 then if in2 > 0 then towardsR seg pt
                                                 else False
                                 else mod in1 2 == 1
        towardsR seg pt = let pt' := (fst pt, -200000)
                              -- true intersection
                              in1 := filter (intersectionND (pt,pt')) seg
                                  |> length |> natToInteger
                              -- false intersection (going through line)
                              in2 := filter (throughSegment (pt,pt')) seg
                                  |> length |> natToInteger
                           in if in1 == 0
                                 then if in2 > 0 then towardsU seg pt
                                                 else False
                                 else mod in1 2 == 1
        towardsU seg pt = let pt' := (200000, snd pt)
                              -- true intersection
                              in1 := filter (intersectionND (pt,pt')) seg
                                  |> length |> natToInteger
                              -- false intersection (going through line)
                              in2 := filter (throughSegment (pt,pt')) seg
                                  |> length |> natToInteger
                           in if in1 == 0
                                 then if in2 > 0 then towardsD seg pt
                                                 else False
                                 else mod in1 2 == 1
        towardsD seg pt = let pt' := (-200000, snd pt)
                              -- true intersection
                              in1 := filter (intersectionND (pt,pt')) seg
                                  |> length |> natToInteger
                              -- false intersection (going through line)
                              in2 := filter (throughSegment (pt,pt')) seg
                                  |> length |> natToInteger
                           in if in1 == 0
                                 then if in2 > 0 then True -- this is not always true...
                                                           -- but all inputs don't have an "interior"
                                                           -- only reachable by a "diagonal"
                                                 else False
                                 else mod in1 2 == 1
        towards : List Segment -> (Integer, Integer) -> Bool
        towards = towardsL -- L -> R -> U -> D

valid : List Nat -> Bool
valid l = count (> 2) l == 4

rectSegments : (Integer, Integer) -> (Integer, Integer) -> List Segment
rectSegments p1 p4 = let p2 := (fst p1, snd p4)
                         p3 := (fst p4, snd p1)
                      in [ (p1,p2), (p2,p4), (p4,p3), (p3,p1)]

rectPoints : (Integer, Integer) -> (Integer, Integer) -> List (Integer, Integer)
rectPoints p1 p4 = let p2 := (fst p1, snd p4)
                       p3 := (fst p4, snd p1)
                    in [p1,p2,p3,p4]

||| filter the valid rectangles
logiq2 : List ((Integer, Integer), (Integer, Integer)) ->
         List Segment ->
         List ((Integer, Integer), (Integer, Integer))
logiq2 cands poly = filter (
                      \(p1,p4) => let points := rectPoints p1 p4
                                             |> filter (pointInside poly)
                                             |> traceVal
                                             |> length
                                             |> ((==) 4)
                                      segmts := rectSegments p1 p4
                                             |> map (countIntersection poly)
                                             |> all (<= 2)
                                   in points && segmts
                    ) cands

record BoundingBox where
  constructor MkBoundingBox
  left : Integer
  right : Integer
  top : Integer
  bottom : Integer

mkBoundingBox : (Integer, Integer) -> (Integer, Integer) -> BoundingBox
mkBoundingBox (c00, c01) (c10, c11) =
  let (left, right) := sortP (c00, c10)
      (bottom, top) := sortP (c01, c11)
   in MkBoundingBox left right top bottom
   -- in MkBoundingBox right left bottom top

AABBCollision : BoundingBox -> BoundingBox -> Bool
AABBCollision (MkBoundingBox l0 r0 t0 b0) (MkBoundingBox l1 r1 t1 b1) =
  l0 < r1 && r0 > l1 && b0 < t1 && t0 > b1

lg1 : ((Integer, Integer), (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer)) -> Bool
-- lg1 (c1,c2) (lc1,lc2) = let sbb := mkBoundingBox c1 c2
--                             lbb := mkBoundingBox lc1 lc2
--                             in AABBCollision sbb lbb
lg1 (c1,c2) (lc1,lc2) = if c1 == lc1 || c1 == lc2 || c2 == lc1 || c2 == lc2
                           then True
                           else let sbb := mkBoundingBox c1 c2
                                    lbb := mkBoundingBox lc1 lc2
                                    in not $ AABBCollision sbb lbb


export
sol2 : String -> String
sol2 = lines
   ||> traverse parseLine
   ||> map (
     pair
     ||> bimap permutationUniq segments
     ||> (\(pts,seg) =>
       pts
       |> sortBy (compare `on` (uncurry area))
       |> reverse
       -- |> map (\p => map (\s =>
       --              -- AABBCollision ((uncurry mkBoundingBox) p) ((uncurry mkBoundingBox) s)
       --              lg1 p s
       --        ) seg)
       -- |> map (\p => all (lg1 p) seg)
       |> find (\p => all (lg1 p) seg)
       |> map (uncurry area)
       |> fromMaybe 0
      )
   )
   ||> maybe "error" show
