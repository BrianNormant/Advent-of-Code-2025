module Solution

import Lib
import Lib.Vect

import Data.List
import Data.List.Extra
import Data.String
import Data.Vect
import Debug.Trace

charToInteger : Char -> Integer
charToInteger '0' = 0
charToInteger '1' = 1
charToInteger '2' = 2
charToInteger '3' = 3
charToInteger '4' = 4
charToInteger '5' = 5
charToInteger '6' = 6
charToInteger '7' = 7
charToInteger '8' = 8
charToInteger '9' = 9
charToInteger _ = 0


export
sol : String -> String
sol = lines
  ||> map (
    unpack
    ||> map charToInteger
--     ||> mapi (\idx, chr => (idx, charToInteger chr))
-- -- what? why the only .snd it finds is from DPair?
--     ||> sortBy (\a,b => compare (Builtin.snd b) (Builtin.snd a))
    -- ||> reverse ||> splitAt 1 ||> (\(last,list) => (reverse list, last))
    ||> foldl ((\(n1, n2),n
      => if n1 * 10 + n2 < n2 * 10 + n
            then (n2, n)
            else if n > n2
                    then (n1, n)
                    else (n1, n2)
      )) (0, 0)
    ||> (\(d,u) => d * 10 + u)
    ||> traceVal
  )
  ||> sum
  ||> show

helper1 : Vect n Integer -> Integer
helper1 l = go $ reverse l where
  go : Vect m Integer -> Integer
  go [] = 0
  go (x::xs) = x + 10 * (go xs)

testH1 : helper1 [1,2,3] = 123
testH1 = Refl

partialPair : (a -> b) -> a -> (a, b)
partialPair f e = bimap id f (pair e)

helper2 : {n : Nat} -> Vect (S n) Integer -> Integer -> Vect (S n) Integer
helper2 l i =
  partialPair (
    removeAtAll ||> map (\ll => ll `snoc` i) ||> foldl max (replicate (S n) 0)
  ) l |> uncurry max
testH2_1 : helper2 [2,3,4,7] 2 === [3,4,7,2]
-- testH2_1 = Refl

export
sol2 : String -> String
sol2 = lines
   ||> map (
     unpack
     ||> map charToInteger
     ||> (\l => toVect' (100) l)
     ||> maybe (-1) (
       Vect.splitAt 12
       ||> uncurry (foldl helper2)
       ||> helper1
       -- ||> traceVal
       )
     )
   ||> sum
   ||> show
