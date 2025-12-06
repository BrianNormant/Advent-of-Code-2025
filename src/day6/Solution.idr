module Solution

import Lib
import Debug.Trace

import Data.DPair
import Data.List
import Data.List1
import Data.String
import Data.Vect

import Decidable.Equality

BinaryOp ty = ty -> ty -> ty

data Operator = Add | Multiply

operator : Char -> Maybe Operator
operator '+' = Just Add
operator '*' = Just Multiply
operator _ = Nothing


parseNumber : List String -> Maybe (n ** Vect n (List Integer))
parseNumber s = let wds := map words s
                    Just f := head' wds | _ => Nothing
                    -- wds@[f::_] := map words s | _ => Nothing
                    l := length f -- number of columns
                 in do tmp <- traverse {t=List,f=Maybe} (traverse parseInteger) wds
                       Just $ fromListDP tmp

parseOperator : String -> Maybe (n ** Vect n Operator)
parseOperator = words
            ||> map unpack
            ||> traverse (head')
            ||> (=<<) (traverse operator)
            ||> map fromListDP

combine : (n ** Vect n (List Integer)) ->
          (m ** Vect m Operator) ->
          Maybe (o ** (Vect o Operator, Vect o (List Integer)))
combine (i ** ints) (j ** ops) =
  case decEq i j of
       Yes Refl => Just (i ** (ops, ints))
       _ => Nothing

parse : String -> Maybe (n ** (Vect n Operator, Vect n (List Integer)))
parse s = let lines := lines s
              S lst_idx := length lines | _ => Nothing
              (numbers, operators) := splitAt lst_idx lines
           in do operators <- head' operators
                 ints <- parseNumber numbers
                 ops <- parseOperator operators
                 combine ints ops

logic1 : (o ** (Vect o Operator, Vect o (List Integer))) -> Integer
logic1 (s ** (ops, ints)) =
  let matched := zip ops ints
   in map (\(op,l) => case op of
                           Add => foldl (+) 0 l
                           Multiply => foldl (*) 1 l
          ) matched
   |> sum

export
sol : String -> String
sol = parse
  ||> maybe "Error Parsing" (logic1 ||> show)

parseNumber2 : List String -> Maybe (n ** Vect n (List Integer))
parseNumber2 =
  map unpack
  ||> transpose
  ||> split (all (== ' '))
  ||> forget
  ||> map (map (map String.singleton))
  ||> map (map (joinBy ""))
  ||> traverse (traverse parseInteger)
  ||> map fromListDP

parse2 : String -> Maybe (n ** (Vect n Operator, Vect n (List Integer)))
parse2 s = let lines := lines s
               S lst_idx := length lines | _ => Nothing
               (numbers, operators) := splitAt lst_idx lines
            in do operators <- head' operators
                  ints <- parseNumber2 numbers
                  ops <- parseOperator operators
                  combine ints ops
export
sol2 : String -> String
sol2 = parse2
   ||> maybe "Error Parsing" (logic1 ||> show)


exVect1 : IO ()
exVect1 = do
  line <- getLine
  putStrLn $ show (Vect.fromList (words line))
