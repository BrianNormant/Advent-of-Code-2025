module Solution

import Lib

import Data.List
import Data.List.Extra
import Data.String
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
  -- ||> const "TODO"
export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
