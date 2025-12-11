module Solution

import Data.String
import Data.List
import Data.List1
import Data.List.Lazy
import Data.Vect
import Data.Fin
import Decidable.Equality

import IO.Async.Loop.Poller
import IO.Async.Loop.Posix
import IO.Async.Signal
import IO.Async.Posix


import Lib
import Lib.LinearAlgebra
import Debug.Trace

import Text.Parser
import Text.Lexer

import Derive.Prelude
%language ElabReflection

data Kind = RBracket
          | LBracket
          | On
          | Off
          | Spaces
          | LParen
          | RParen
          | Number
          | Comma
          | RCurly
          | LCurly
          | Newline

%runElab derive "Kind" [Eq]

TokenKind Kind where
  TokType On       = Bool
  TokType Off      = Bool
  TokType Number   = Integer
  TokType RBracket = ()
  TokType LBracket = ()
  TokType Spaces   = ()
  TokType LParen   = ()
  TokType RParen   = ()
  TokType Comma    = ()
  TokType RCurly   = ()
  TokType LCurly   = ()
  TokType Newline  = ()
  tokValue On _       = True
  tokValue Off _      = False
  tokValue Number n   = cast n
  tokValue RBracket _ = ()
  tokValue LBracket _ = ()
  tokValue Spaces _   = ()
  tokValue LParen _   = ()
  tokValue RParen _   = ()
  tokValue Comma _    = ()
  tokValue RCurly _   = ()
  tokValue LCurly _   = ()
  tokValue Newline _  = ()

tmap : TokenMap (Token Kind)
tmap = [
  (newline, Tok Newline),
  (exact "#", Tok On),
  (exact ".", Tok Off),
  (digits, Tok Number),
  (exact "]", Tok RBracket),
  (exact "[", Tok LBracket),
  (spaces, Tok Spaces),
  (exact "(", Tok LParen),
  (exact ")", Tok RParen),
  (exact ",", Tok Comma),
  (exact "}", Tok RCurly),
  (exact "{", Tok LCurly)
  ]

indicator : Grammar state (Token Kind) True (n ** Vect n Bool)
indicator = do match LBracket
               l <- some light
               match RBracket
               pure $ Vect.fromListDP $List1.forget l
  where
    light : Grammar state (Token Kind) True Bool
    light = choice (the (List ?) [(match On),(match Off)])


parseFin : (n:Nat) -> Grammar state (Token Kind) True (Fin n)
parseFin f = terminal "interToFinFailed" $
      \t => if t.kind == Number
               then integerToFin (tokValue Number t.text) f
               else Nothing

button : (n:Nat) -> Grammar state (Token Kind) True (List1 (Fin n))
button f = do match LParen
              l <- sepBy1 (match Comma) (parseFin f)
              match RParen
              pure l

joltage : Grammar state (Token Kind) True (List1 Integer)
joltage = do match LCurly
             l <- sepBy1 (match Comma) (match Number)
             match RCurly
             pure $ l

data Diagram : Nat -> Type where
  MkDiagram : {n:Nat} -> (v : Vect n Bool) -> (l : List1 (List1 (Fin n))) -> Diagram n

Show (Diagram n) where
  show (MkDiagram v l) = let v := map (\b => if b then "#" else ".") v
                               |> toList
                               |> joinBy ""
                               |> (\s => "[" ++ s ++ "]")
                          in v ++ show l

line : Grammar state (Token Kind) True (n ** Diagram n)
line = do req <- indicator
          helper1 req
  where
    helper1 : (n ** Vect n Bool) -> Grammar state (Token Kind) True (n ** Diagram n)
    helper1 (Z ** _) = fail "impossible"
    helper1 (n@(S _) ** v) = do match Spaces
                                buttons <- sepBy1 (match Spaces) (button n)
                                match Spaces
                                _ <- joltage
                                pure $ (n ** MkDiagram {n=n} v buttons)

text : Grammar state (Token Kind) True (List1 ( n ** Diagram n))
text = sepBy1 (match Newline) line


parseAll : String -> Maybe (List (n ** Diagram n))
parseAll s = lex tmap s
         |> fst
         |> parse text
         |> either (const (the (Maybe (List (n ** Diagram n))) Nothing)) go
  where
    go : (List1 (n : Nat ** Diagram n), List (WithBounds (Token Kind))) -> Maybe (List (n : Nat ** Diagram n))
    go (l1, _) = Just $ forget l1

||| state of a machine
data Machine : Nat -> Type where
  MkMachine : Vect n Bool -> Machine n

initMachine : Diagram n -> Machine n
initMachine (MkDiagram _ _) = MkMachine $ replicate n False

||| press a button changes the state of the machine
press : {n:Nat} -> Machine n -> List1 (Fin n) -> Machine n
press (MkMachine v) l1 =
       let upd := foldl {t=List1} (\v',idx => toggle v' idx) v l1
        in MkMachine upd
  where
    toggle : Vect m Bool -> Fin m -> Vect m Bool
    toggle v idx = updateAt idx not v

||| press all button listed in order
pressCombi : {n : Nat} -> Machine n -> List (List1 (Fin n)) -> Machine n
pressCombi = foldl press

||| a machine is in the correct configuration when it matches it's diagram
validMachine : {n:Nat} -> Machine n -> Diagram n -> Bool
validMachine (MkMachine v1) (MkDiagram v2 _) = v1 == v2

allTests : LazyList Nat
allTests = iterate fn 1
  where
    fn : Nat -> Maybe Nat
    fn n = if n < 10000
              then Just $ S n
              else Nothing

||| test every combination of button press of a machine
||| return the number of required button press to reach a valid state
testMachine : Diagram n -> Nat
testMachine d@(MkDiagram v bts) =
  let tests := allTests
            |> dropWhile (\n => Lazy.all (\combi =>
                               let machine := initMachine d
                                   machine := pressCombi machine combi
                                   in not $ validMachine machine d
                                    ) (combiLazy n (forget bts))
                         )
   in fromMaybe 0 $ head' tests

export
sol : String -> String
sol = parseAll
    ||> maybe "parse error" (
      map (\(n ** d) => testMachine d)
      ||> sum
      ||> show
      )

str1 : String
str1 = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"

solTest : String
solTest = sol "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"

data Diagram2 : Nat -> Type where
  MkDiagram2 : {n:Nat} -> (v : Vect n Integer) -> (l : List1 (List1 (Fin n))) -> Diagram2 n

Show (Diagram2 n) where
  show (MkDiagram2 v l) = let v := map (\b => cast b) v
                                |> toList
                                |> joinBy "|"
                                |> (\s => "[" ++ s ++ "]")
                           in v ++ show l


joltage2 : (n:Nat) -> Grammar state (Token Kind) True (Vect n Integer)
joltage2 n = do match LCurly
                l <- sepBy1 (match Comma) (match Number)
                helper n l
  where
    helper : (n:Nat) -> List1 Integer -> Grammar state (Token Kind) True (Vect n Integer)
    helper Z _ = fail "impossible"
    helper n l with (toVect {a=Integer} n $ forget l)
      _ | Just v = do match RCurly
                      Core.pure $ v
      _ | Nothing = fail "wrong length"




line2 : Grammar state (Token Kind) True (n ** Diagram2 n)
line2 = do req <- indicator
           helper2 req
  where
    helper2 : (n ** Vect n Bool) -> Grammar state (Token Kind) True (n ** Diagram2 n)
    helper2 (Z ** _) = fail "impossible"
    helper2 (n@(S _) ** v) = do match Spaces
                                buttons <- sepBy1 (match Spaces) (button n)
                                match Spaces
                                v' <- joltage2 n
                                pure $ (n ** MkDiagram2 {n=n} v' buttons)

text2 : Grammar state (Token Kind) True (List1 ( n ** Diagram2 n))
text2 = sepBy1 (match Newline) line2


parseAll2 : String -> Maybe (List (n ** Diagram2 n))
parseAll2 s = lex tmap s
          |> fst
          |> parse text2
          |> either (const Nothing) go
  where
    go : (List1 (n : Nat ** Diagram2 n), List (WithBounds (Token Kind))) -> Maybe (List (n : Nat ** Diagram2 n))
    go (l1, _) = Just $ forget l1

||| state of a machine
data Machine2 : Nat -> Type where
  MkMachine2 : Vect n Integer -> Machine2 n

initMachine2 : Diagram2 n -> Machine2 n
initMachine2 (MkDiagram2 _ _) = MkMachine2 $ replicate n 0

||| press a button changes the state of the machine
||| pressing the button increase the value of each "register"
press2 : {n:Nat} -> Machine2 n -> List1 (Fin n) -> Machine2 n
press2 (MkMachine2 v) l1 =
        let upd := foldl {t=List1} (\v',idx => toggle v' idx) v l1
         in MkMachine2 upd
  where
    toggle : Vect m Integer -> Fin m -> Vect m Integer
    toggle v idx = updateAt idx (+1) v

||| press all button listed in order
pressCombi2 : {n : Nat} -> Machine2 n -> List (List1 (Fin n)) -> Machine2 n
pressCombi2 = foldl press2

||| a machine is in the correct configuration when it matches it's diagram
validMachine2 : {n:Nat} -> Machine2 n -> Diagram2 n -> Bool
validMachine2 (MkMachine2 v1) (MkDiagram2 v2 _) = v1 == v2


||| It's linear algebra. Each button list correspond to a list of vector
||| If we can expression the solution, ie the diagram joltage
||| As a linear combination of those vectors,
||| we have a valid subset. The Sum of factor is the score of this particular vector subset

||| The algorithm will be implemented as follow:
||| We generate all combinations for the vectors

||| a button is a n sized vector
btnToVect : {n:Nat} -> List1 (Fin n) -> Vect n Integer
btnToVect btn = go (forget btn) $ replicate n 0
  where
    go : List (Fin n) -> Vect n Integer -> Vect n Integer
    go [] v = v
    go (x::xs) v = go xs $ replaceAt x 1 v

augmentedMat : Vect m (Vect n Integer) -> Vect n Integer -> Vect (S m) (Vect n Integer)
augmentedMat = snoc

constructMat : Diagram2 n -> (m ** Vect m (Vect n Integer))
constructMat (MkDiagram2 j btn) = map btnToVect btn
                               |> (flip List1.snoc) j
                               |> forget
                               |> fromListDP


matToDouble : Vect m (Vect n Integer) -> Vect m (Vect n Double)
matToDouble = map (map cast)
matToInteger : Vect m (Vect n Double) -> Vect m (Vect n Integer)
matToInteger = map (map cast)
-- matToInteger = map (map (\dbl => if dbl < 0 then 0 else cast dbl ))

doubleEq : Double -> Double -> Bool
doubleEq a b = (abs $ a - b) < 0.00001

countMat : {n,m:Nat} -> Vect m (Vect n Integer) -> Maybe Integer
countMat = matToDouble
           ||> ref
           -- ||> matToInteger
           -- ||> traceVal
           ||> Vect.transpose -- we study rows
           ||> traverse (\row =>
               do init <- init' $ toList row
                  last <- last' $ toList row
                  if (all (doubleEq 0) init) && (not $ doubleEq 0 last)
                     then Nothing
                     else if last < 0 || doubleEq 0 (last - floor last)
                       then Nothing
                       else Just $ cast last
                        )
           ||> map sum


||| test every combination of button press of a machine
||| return the number of required button press to reach a valid state
testMachine2 : Diagram2 m -> Integer
testMachine2 d@(MkDiagram2 v bts) =
  let subsets := subsets (forget bts)
              |> filter (\s => length s >= 2)
   in map (\subset =>
           let asMat := map btnToVect subset
               asMat := asMat `snoc` v
                     |> fromListDP
            in case count' asMat of
                    Just n => n
                    Nothing => 2000000
            ) subsets
      |> foldl (\a,b => if a < b then a else b) 999999
  where
    count' : (n ** Vect n (Vect m Integer)) -> Maybe Integer
    count' (_ ** vv) = countMat vv

export
sol2 : String -> String
sol2 = parseAll2
     ||> maybe "parse error" (
       map (\(n ** d) => testMachine2 d)
       ||> sum
       ||> show
       )
