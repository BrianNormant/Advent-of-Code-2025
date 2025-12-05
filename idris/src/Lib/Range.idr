module Lib.Range

import Lib

||| A inclusive range of integers
||| continuous between from and to
public export
record Range where
  constructor MkRange
  from : Integer
  snd : Integer

public export
Show Range where
  show (MkRange f t) = "(" ++ show f ++ ", " ++ show t ++ ")"

||| Verify if a number is in a range
export
inside : Integer -> Range -> Bool
inside n (MkRange f t) = f <= n && n <= t

export
length : Range -> Nat
length (MkRange f t) = cast $ t - f + 1

export
||| Order two ranges
||| the range that start first is first
||| if the 2 range start at the same time, the range that ends first is first
order : (Range, Range) -> (Range, Range)
order (MkRange f t, MkRange f' t') =
  if f < f' then (MkRange f t, MkRange f' t')
            else if f == f'
                    then if t < t' then (MkRange f t, MkRange f' t')
                                   else (MkRange f' t', MkRange f t)
                    else (MkRange f' t', MkRange f t)

order_T1 : order (MkRange 1 2, MkRange 3 4) = (MkRange 1 2, MkRange 3 4)
order_T1 = Refl
order_T2 : order (MkRange 1 2, MkRange 2 3) = (MkRange 1 2, MkRange 2 3)
order_T2 = Refl
order_T3 : order (MkRange 1 2, MkRange 1 3) = (MkRange 1 2, MkRange 1 3)
order_T3 = Refl
order_T4 : order (MkRange 1 2, MkRange 1 2) = (MkRange 1 2, MkRange 1 2)
order_T4 = Refl
order_T5 : order (MkRange 3 4, MkRange 1 2) = (MkRange 1 2, MkRange 3 4)
order_T5 = Refl
order_T6 : order (MkRange 1 5, MkRange 1 3) = (MkRange 1 3, MkRange 1 5)
order_T6 = Refl

||| Union two ranges
export
union : Range -> Range -> Maybe Range
union r1 r2 = let (r, r') := order (r1, r2)
                  MkRange f t := r
                  MkRange f' t' := r'
               in if (t + 1) >= f'
                     then pure $ MkRange f (max t' t)
                     else empty

union_T1 : union (MkRange 1 2) (MkRange 3 4) = Just (MkRange 1 4)
union_T1 = Refl
union_T2 : union (MkRange 1 2) (MkRange 4 10) = Nothing
union_T2 = Refl
union_T3 : union (MkRange 1 5) (MkRange 2 3) = Just (MkRange 1 5)
union_T3 = Refl
union_T4 : union (MkRange 1 5) (MkRange 2 8) = Just (MkRange 1 8)
union_T4 = Refl
union_T5 : union (MkRange 10 14) (MkRange 12 18) = Just (MkRange 10 18)
union_T5 = Refl

||| Merge two ranges,
||| if the range range overlaps, return the resulting union
||| if the ranges are disjoint, return the 2 ranges
export
merge : Range -> Range -> Either (Range) (Range, Range)
merge r r' = case union r r' of
                  Just r'' => Left r''
                  Nothing => Right (r, r')
