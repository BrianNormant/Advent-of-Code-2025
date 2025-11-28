module Lib.Grid

import Data.Vect
import Data.List

import Lib.Vect
import Lib.List
import Lib.Fin

import Derive.Prelude

%default total

%language ElabReflection

public export
record Dimension where
  constructor MkDim
  dimx : Nat
  dimy : Nat

%runElab derive "Dimension" [Eq, Show]

public export
Grid : Dimension -> Type -> Type
Grid dim a = Vect dim.dimy (Vect dim.dimx a)
-- public export
-- data Grid : (dim : Dimension) -> (elem : Type) -> Type where
--   NGrid : Vect dim.dimy (Vect dim.dimx a)

fromVects : {n,m : Nat} -> Vect m (Vect n a) -> Grid (MkDim n m) a
fromVects v = v


public export
record Coord (dim : Dimension) where
  constructor MkCoord
  px : Fin dim.dimx
  py : Fin dim.dimy

public export
Show (Coord dim) where
  show (MkCoord x y) = "(MkCoord " ++ show x ++ " " ++ show y ++ ")"

export
||| the coordinate above
coordU : {dim : Dimension} -> Coord dim -> Maybe (Coord dim)
coordU (MkCoord x y) = do
  y <- inc y
  pure $ MkCoord x y

export
||| the coordinate below
coordD : {dim : Dimension} -> Coord dim -> Maybe (Coord dim)
coordD (MkCoord x y) = do
  y <- dec y
  pure $ MkCoord x y

export
||| the coordinate right
coordR : {dim : Dimension} -> Coord dim -> Maybe (Coord dim)
coordR (MkCoord x y) = do
  x <- inc x
  pure $ MkCoord x y

export
||| the coordinate left
coordL : {dim : Dimension} -> Coord dim -> Maybe (Coord dim)
coordL (MkCoord x y) = do
  x <- dec x
  pure $ MkCoord x y

export
index : {dim : Dimension} -> Coord dim -> Grid dim a -> a
index (MkCoord x y) g = indexMat (x, y) g

export
replaceAt : {dim : Dimension} -> (Coord dim) -> a -> (Grid dim a) -> (Grid dim a)
replaceAt (MkCoord x y) a g = let lin = Vect.index y g
                                  lin = Vect.replaceAt x a lin
                               in Vect.replaceAt y lin g

export
update : {dim : Dimension} -> Coord dim -> (a -> a) -> Grid dim a -> Grid dim a
update c f g = let old = index c g
                   new = f old
                in replaceAt c new g

export
Eq (Coord dim) where
  (MkCoord x y) == (MkCoord x' y') = x == x' && y == y'

export
||| create a grid from a list of lists
fromLists : List (List a) -> (dim ** Grid dim a)
fromLists [] = (MkDim 0 0 ** Vect.Nil)
fromLists ll@([] :: _) =
  let dim = MkDim 0 (length ll)
      v = Vect.replicate (dim.dimy) $
          the (Vect 0 a) Vect.Nil
   in (dim ** believe_me v)
fromLists ll@((_::_)::_) =
  let y = length ll
      fst = head ll
      v = map (toVect (length fst)) ll |> catMaybes
      v = Vect.fromList v
   in ((MkDim (length fst) (length v)) ** believe_me v)

export
map : (a -> b) -> (Grid dim a) -> Grid dim b
map f = map $ map f

export
find : {dim : Dimension} -> (a -> Bool) -> (Grid dim a) -> Maybe a
find {dim = MkDim (S dx) (S dy)} f m@(x::xs) with (find f x)
  _ | Just n = Just n
  _ | Nothing = assert_total $ Grid.find {dim = MkDim (S dx) dy}f (believe_me xs)
find _ _ = Nothing

export
||| zip each element with its position in the matrix
zipWithIndex : {dim : Dimension} -> Grid dim a -> Grid dim (Coord dim, a)
zipWithIndex g = zipWith (\j, vs =>
                 zipWith (\i, a => (MkCoord i j, a)) (allFins dim.dimx) vs
                 ) (allFins dim.dimy) g



export
||| vertical and horizontal neighbors as values + coordinates
neighbors : {dim : Dimension} -> Coord dim -> Grid dim a -> List (a, Coord dim)
neighbors c g = [coordU c, coordD c,
                 coordL c, coordR c]
                |> List.catMaybes
                |> map (\c => (Grid.index c g, c))

export
||| forget the fact that this is a grid
devolve : {dim : Dimension} -> Grid dim a -> List (List a)
devolve g = toList (map toList g)
