module Solution

import Data.String
import Data.List
import Data.List1

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

export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
