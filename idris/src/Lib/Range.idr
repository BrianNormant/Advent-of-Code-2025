module Lib.Range

import Lib

||| A inclusive range of integers
public export
record Range where
  constructor MkRange
  from : Integer
  snd : Integer

||| Verify if a number is in a range
export
inside : Integer -> Range -> Bool
inside n (MkRange f t) = f <= n && n <= t
