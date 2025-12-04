module Solution

import Data.List
import Data.Maybe
import Data.String
import Data.Vect

import Lib
import Lib.Vect

import Debug.Trace

data Roll = Empty | Here

Show Roll where
  show Empty = "."
  show Here = "@"
Eq Roll where
  Empty == Empty = True
  Here == Here = True
  _ == _ = False

charToRoll : Char -> Roll
charToRoll '@' = Here
charToRoll _ = Empty

||| parse the grid
parseGrid : {n,m: Nat} -> String -> Maybe (Vect n (Vect m Roll))
parseGrid = lines
        ||> map (
          unpack
          ||> map charToRoll
          ||> toVect m
          )
        ||> catMaybes
        ||> toVect n

export
sol : String -> String
sol s = let l := lines s
            n := length l
            m := maybe 0 length (head' l)
         in parseGrid {n=n,m=m} s
         |> maybe "Error parsing" (
            pair
            ||> (rightPair allCoord)
            ||> (\(mat, coords) =>
                filter (\c => (indexMat c mat) == Here ) coords
                |> map (\c => (allNeighbors (swap c) mat))
                )
            ||> map (map fst ||> filter (== Here) ||> length ||> natToInteger)
            ||> filter (< 4)
            ||> length
            ||> show
            )

||| find the rolls that can be removed
findRemovableRolls : {n,m: Nat} -> Vect n (Vect m Roll) -> List (Fin n, Fin m)
findRemovableRolls =
  pair
  ||> (rightPair allCoord)
  ||> (\(mat, coords) =>
        filter (\c => (indexMat c mat) == Here ) coords
        |> map (\c => (
               allNeighbors (swap c) mat
               |> filter (\(r,_) => r == Here)
               |> length
               |> MkPair c
        ))
        |> filter (\(_, l) => l < 4)
        |> map (swap . fst)
      )

||| find the rolls that can be removed
||| remove them
||| return the updated grid
removeRolls : {n,m:Nat} -> Vect n (Vect m Roll) -> Vect n (Vect m Roll)
removeRolls mat = let removable := findRemovableRolls mat
                   in remove removable mat where
  remove : List (Fin n, Fin m) -> Vect n (Vect m Roll) -> Vect n (Vect m Roll)
  remove [] mat = mat
  remove (x::xs) mat = remove xs (replaceMat (swap x) mat Empty)

countRolls : {n,m:Nat} -> Vect n (Vect m Roll) -> Nat
countRolls = toListMat ||> filter (== Here) ||> length

MAX_ITERATION : Nat
MAX_ITERATION = 1000

export
sol2 : String -> String
sol2 s = let l := lines s
             n := length l
             m := maybe 0 length (head' l)
          in parseGrid {n=n,m=m} s
          |> maybe "Error parsing" (
              pair
              ||> bimap (
                untilSameResult' MAX_ITERATION removeRolls countRolls
                ||> snd
                ) countRolls
              ||> bimap (map natToInteger) natToInteger
              ||> (\(l, i) =>
                    foldr (\current,(last,removed)
                          => (current, removed + (last - current)) ) (i,0) l
                  )
              ||> snd
              ||> show
             )
