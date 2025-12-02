module Solution

import Lib
import Lib.List

import Data.List.Lazy

import Text.Parser
import Text.Lexer

import Debug.Trace

import Derive.Prelude
%language ElabReflection

data KindType = CommaType | RangeType
data Kind = Number | Comma | Range

%runElab derive "Kind" [Eq]

implementation TokenKind Kind where
  TokType Number = Integer
  TokType Comma = KindType
  TokType Range = KindType

  tokValue Number n = cast n
  tokValue Comma _ = CommaType
  tokValue Range _ = RangeType

tmap : TokenMap (Token Kind)
tmap = [
  (digits, Tok Number),
  (exact ",", Tok Comma),
  (exact "-", Tok Range)
  ]


rangeGrammar : Grammar state (Token Kind) ?complr (Integer, Integer)
rangeGrammar = do n1 <- match Number
                  _ <- match Range
                  n2 <- match Number
                  pure (n1, n2)

textGrammar : Grammar state (Token Kind) ?compl (List (Integer, Integer))
textGrammar = sepBy (match Comma) rangeGrammar

||| lazily iterate on all the value from f to t inclusive
lazyRange : (Integer, Integer) -> LazyList (Integer)
lazyRange (f, t) = iterate (\m => if (m+1) <= t then Just (m+1) else Nothing) f

lazyRangeNat : (Nat, Nat) -> LazyList (Nat)
lazyRangeNat (f, t) = iterate (\m => if (m+1) <= t then Just (m+1) else Nothing) f

ex : String
ex = "11-22"

fn : (Integer, Integer) -> Integer
fn r = foldl
  (\acc, n => let l := show n |> unpack
                  s := div (length l) 2
                  p1 := take s l
                  p2 := drop s l
                  in if p1 == p2 then acc + n else acc
  ) 0 (lazyRange r)

export
sol : String -> String
sol = lex tmap
  ||> fst
  ||> parse textGrammar
  ||> either (const "error parsing") (
    fst ||> map fn ||> sum ||> show
  )

allEq : Eq a => List a -> Bool
allEq [] = False
allEq [_] = True
allEq [x,y] = x == y
allEq (x::xs@(y::ys)) = if x == y then allEq xs
                                  else False

fn2 : (Integer, Integer) -> Integer
fn2 r = foldl
  (\acc, n => let l := show n |> unpack
                  lr := lazyRangeNat (2, length l)
                  m := toList $ map ( (flip divideL) l) lr
                    |> filter allEq
                    |> toList
                    |> join
                  in if (length m) >= 1 then acc + n else acc
  ) 0 (lazyRange r)

export
sol2 : String -> String
sol2 = lex tmap
  ||> fst
  ||> parse textGrammar
  ||> either (const "error parsing") (
    fst ||> map fn2 ||> sum ||> show
  )
