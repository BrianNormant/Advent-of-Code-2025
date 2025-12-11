module Lib.LinearAlgebra

import Lib.Vect
import Lib.List
import Data.List
import Data.Vect

import Debug.Trace

mattest : Vect 3 (Vect 3 Double)
mattest = [[1,2,3],[4,5,6],[7,8,9]]
mattest2 : Vect 3 (Vect 3 Double)
mattest2 = [[1,0,0],[4,-3,-6],[7,-6,-12]]

mattest3 : Vect ? (Vect ? Double)
mattest3 = [[0,0,0,1],
            [0,1,0,1],
            [0,0,1,1],
            [1,0,1,0],
            [1,1,0,0],
            [3,5,4,7]]

mattest4 : Vect ? (Vect ? Double)
mattest4 = [[1,0,1,1,1],
            [0,0,1,1,0],
            [1,0,1,0,0],
            [7,5,12,7,2]]

doubleEq : Double -> Double -> Bool
doubleEq x y = abs (x - y) < 0.00001
doubleNeq : Double -> Double -> Bool
doubleNeq x y = abs (x - y) >= 0.00001


--- pivot [0,0,0] False
--- pivot [0,1,0] False
--- pivot [0,0,1] False
--- pivot [0,2,0] True
--- pivot [0,1,1] True
pivot : Vect m Double -> Bool
pivot c = case findIndex (doubleNeq 0) c of
               Nothing => False
               Just idx => let x := index idx c
                               xs := toList c
                                  |> List.splitAt (finToNat idx)
                                  |> snd
                                  |> tail'
                            in case xs of
                                    Just [] => doubleNeq x 1
                                    Just xs => doubleNeq x 1 || any (doubleNeq 0) xs
                                    Nothing => doubleNeq x 1
||| find row echelon form
||| internal algo, this only represent a step
||| Vector of Columns, not row
||| ie : n=0 correspond the the first column
|||      m=0 correspond to the first row
ref_int : {n,m:Nat} -> Vect n (Vect m Double) -> List (Fin m) -> (Vect n (Vect m Double), List (Fin m))
ref_int mat pivots_ignored =
  let Just (pivot_col, pivot_row) := findPivot mat (pivots_ignored)
                                   | Nothing => (mat, pivots_ignored)
      Just zero_r := natToFin 0 m | Nothing => (mat, pivots_ignored)
      Just zero_c := natToFin 0 n | Nothing => (mat, pivots_ignored)
      -- mat := map (\c => let tmp := index zero_r c
      --                       piv := index pivot_row c
      --                       c := replaceAt zero_r piv c
      --                       c := replaceAt pivot_row tmp c
      --                    in c
      --            ) mat
      --- scale the pivot row to 1
      pivot_v := index pivot_row $ index pivot_col mat
      mat := map (\c => let tmp := index pivot_row c
                            tmp := tmp / pivot_v
                         in replaceAt pivot_row tmp c
                 ) mat
      --------------- eliminate below
      -- [multiple for each row]
      mul := index pivot_col mat
             |> replaceAt pivot_row 0
      mat := map (\c =>
            let piv := index pivot_row c
             in mapi (
               \cr,idxm => cr - ((index idxm mul) * piv)
              ) c
                 ) mat
   in (mat, pivot_row :: pivots_ignored)
  where
    factor : Vect m' Double -> Vect m' Double
    factor [] = []
    factor [x] = [0] -- pivot is in the first row, and we don't want to eliminate it
    factor (x::xs) = (- x) :: (factor xs)
    findPivot : Vect n (Vect m Double) -> List (Fin m) -> Maybe (Fin n, Fin m)
    findPivot mat ignored = zipWithIndexMat mat
                          |> toList
                          |> map toList
                          |> join
                          |> (\l => (l, map pivot mat))
                          |> (\(l,p) =>
                               filter (\((fm,fn),d) =>
                                (doubleNeq 0 d)
                                -- && (index fn p)
                                && (not $ ignored `contains` fm)
                               ) l
                             )
                          |> map fst
                          |> map swap
                          |> head'

-- reduced row echelon form
export
ref : {n,m:Nat} -> Vect n (Vect m Double) -> Vect n (Vect m Double)
ref mat = go 1000 mat []
 where
   go : {n',m':Nat} -> Nat -> Vect n' (Vect m' Double) -> List (Fin m') -> Vect n' (Vect m' Double)
   go Z mat ignored = mat
   go (S n) mat ignored = if (length ignored) == m'
                             then mat
                             else let (mat', ignored') := ref_int mat ignored
                                   in go n mat' ignored'
