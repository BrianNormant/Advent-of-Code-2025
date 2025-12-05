module Solution

import Lib
import Lib.Range

import Debug.Trace

import Data.List

import Text.Lexer
import Text.Parser

import Derive.Prelude
%language ElabReflection

data Kind = Number
          | Hyphen
          | NewLine

%runElab derive "Kind" [Eq]

TokenKind Kind where
  TokType Number = Integer
  TokType Hyphen = ()
  TokType NewLine = ()

  tokValue Number n = cast n
  tokValue Hyphen _ = ()
  tokValue NewLine _ = ()

tmap : TokenMap (Token Kind)
tmap = [
  (digits, Tok Number),
  (exact "-", Tok Hyphen),
  (newline, Tok NewLine)
  ]

rangeGrammar : Grammar state (Token Kind) True Range
rangeGrammar = do f <- match Number
                  match Hyphen
                  t <- match Number
                  pure $ MkRange f t


inputGrammar : Grammar state (Token Kind) True (List Range, List Integer)
inputGrammar = do ranges <- sepBy1 (match NewLine) rangeGrammar
                  _ <- some (match NewLine)
                  list <- sepBy1 (match NewLine) (match Number)
                  pure $ bimap forget forget (ranges, list)

export
sol : String -> String
sol = lex tmap
  ||> fst
  ||> parse inputGrammar
  ||> either (const "parsing error") (
    fst
    ||> (\(ranges, list) =>
          map (\n => any (inside n) ranges) list
        )
    ||> filter id
    ||> length
    ||> show
    )

mergeRanges : List Range -> Range -> List Range
mergeRanges l r = last' l
               |> maybe [r] (
                 \last => let before : List Range
                              before = fromMaybe [] $ tail' $ reverse l
                           in case merge last r of
                                   Left last' => before `snoc` last'
                                   Right _ => l `snoc` r
                 )

export
sol2 : String -> String
sol2 = lex tmap
   ||> fst
   ||> parse inputGrammar
   ||> either (const "parsing error") (
     fst ||> fst
     ||> sortBy (compare `on` from)
     ||> foldl mergeRanges []
     ||> map length
     ||> sum
     ||> show
     )
