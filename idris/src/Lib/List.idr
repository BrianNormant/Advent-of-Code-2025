module Lib.List

import Data.List
import Data.List1
import Data.Fin
import Data.Integral

%default total

||| Decide if a list is non empty
public export
isNonEmpty : (l : List a) -> Dec (NonEmpty l)
isNonEmpty [] = No uninhabited
isNonEmpty (_::_) = Yes IsNonEmpty

reversePreservesNonEmpty : {l : List a} -> NonEmpty l -> NonEmpty (reverseOnto [] l)
reversePreservesNonEmpty (IsNonEmpty {x} {xs}) =
  helper [x] xs where
     helper : (i : List a) -> (j : List a) ->
              {auto 0 ok : NonEmpty i} ->
              NonEmpty (reverseOnto i j)
     helper [] [] impossible
     helper (x::_) [] = IsNonEmpty
     helper li (y::ys) = helper (y::li) ys

reverseIsReverseOnto : {l : List a} -> reverse l = reverseOnto [] l
reverseIsReverseOnto = Refl

dropLast : (l : List a) -> {auto 0 ok : NonEmpty l} -> List a
dropLast l = reverse $ tail (reverseOnto [] l) {ok = reversePreservesNonEmpty ok}

||| Same as index'
||| Construct a new list consisting of all but the indicated element.
||| But use Fin instead of a proof
export
deleteAt' : (xs : List a) -> Fin (length xs) -> List a
deleteAt' (_::xs) FZ = xs
deleteAt' (x::xs) (FS i) = x :: ( deleteAt' xs i )

||| Same as index'
||| Replace an element at a particlar index with another.
||| But uses Fin instead of a proof
export
replaceAt' : (xs : List a) -> Fin (length xs) -> a -> List a
replaceAt' (_ :: xs) FZ y = y :: xs
replaceAt' (x :: xs) (FS k) y = x :: replaceAt' xs k y

||| Same as index'
||| Insert an element at a particular index.
||| But uses Fin instead of a proof
export
insertAt' : (l : List a) -> Fin (length l) -> a -> List a
insertAt' [] _ a impossible
insertAt' (x::xs) FZ a = a :: x :: xs
insertAt' (x::xs) (FS k) a = x :: insertAt' xs k a

||| Same as index'
||| Insert an element just after a particular index.
||| But uses Fin instead of a proof
export
insertAfterAt' : (l : List a) -> Fin (length l) -> a -> List a
insertAfterAt' [] _ a impossible
insertAfterAt' (x::xs) FZ a = a :: x :: xs
insertAfterAt' (x::xs) (FS k) a = x :: insertAfterAt' xs k a

||| try to index a List, if the index is out of bound return Nothing
export
indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (x :: _) = Just x
indexMaybe (S i) (_::xs) = indexMaybe i xs
indexMaybe _ [] = Nothing

export
deleteMaybe : Nat -> List a -> Maybe (List a)
deleteMaybe _ [] = Nothing
deleteMaybe Z (x :: xs) = Just xs
deleteMaybe (S k) (x :: xs) = deleteMaybe k xs |>
                              map (\r => x :: r)
export
updateMaybe : Nat -> (a -> a) -> List a -> Maybe (List a)
updateMaybe Z _ [] = Just []
updateMaybe (S k) _ [] = Nothing
updateMaybe Z f (x :: xs) = Just $ f x :: xs
updateMaybe (S k) f (x :: xs) = let new = updateMaybe k f xs in map (\t => x :: t) new

export
||| take the middle element of list
middle : List a -> Maybe a
middle [] = Nothing
middle [x] = Just x
middle l = if (odd (length l))
              then indexMaybe ((length l) `div` 2) l
              else Nothing

export
||| swap to element by their index in a List
||| @arg l list to swap on
||| @arg idx1, idx2 index to swap
swapAt' : (l : List a) -> Fin (length l) -> Fin (length l) -> List a
swapAt' [] _ _ = []
swapAt' l x y = let a = index' l x
                    b = index' l y
                 in replaceAt' l x b
                 --- I'd need a proof that the lenght of the original
                 --- list is the same that the lenght of the intermediary list
                 |> (\l => replaceAt' l (believe_me y) a)

export
||| swap the first element of a list that respect a predicate
||| with a different element
||| ex: swap (== 0) [1,0,3,4] 9 := [1,9,3,4]
swapIf : (a -> Bool) -> a -> List a -> List a
swapIf _ _ [] = []
swapIf p a (x::xs) with (p x)
  _ | True = a :: xs
  _ | False = x :: (swapIf p a xs)

export
splitPairs : List (a, b) -> (List a, List b)
splitPairs [] = ([], [])
splitPairs ((a, b)::t) = let (xs, ys) = splitPairs t in (a ::xs, b :: ys)

export
mapif : (a -> Bool) -> (a -> a) -> List a -> List a
mapif _ _ [] = []
mapif p f (x::xs) = (if (p x) then f x else x ) :: mapif p f xs

export
||| it is often necessary to first sort before grouping elements
groupSort : Eq a => Ord a => List a -> List (List a)
groupSort l = sort l |> group |> map forget

export
||| sort then group, uses a function that transform the contained
||| datatype into a comparable and sortable one
sortGroupBy : Eq b => Ord b => (a -> b) -> List a -> List (List a)
sortGroupBy f l = sortBy (\x,y  => compare (f x) (f y)) l
               |> groupBy (\x,y => (f x) == (f y))
               |> map forget

||| splitAt can only return a smaller list
||| So we can ascertain that function will converge to the empty list case
covering
divideSplit : Nat -> List a -> List (List a)
divideSplit Z _ = []
divideSplit _ [] = []
divideSplit n@(S _) l = let (l1, l2) = splitAt n l in
                            l1 :: divideSplit n l2


covering
export
||| divide a list in n equal parts, if the list is not divisible
||| by n return the empty list
divideL : Nat -> List a -> List (List a)
divideL Z _ = []
divideL 1 l = [l]
divideL n l = if mod (length l) n == 0
                 then divideSplit (div (length l) n) l
                 else []


