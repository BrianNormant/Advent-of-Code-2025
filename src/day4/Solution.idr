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

export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
