module Solution

import Data.String
import Data.List
import Data.List1
import Data.List.Lazy
import Data.Vect
import Data.Fin
import Decidable.Equality


import Lib
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

export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
