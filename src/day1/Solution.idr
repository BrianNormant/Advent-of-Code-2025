module Solution

import Text.Parser
import Text.Lexer

import Data.Either

import Debug.Trace

import Lib

import Derive.Prelude
%language ElabReflection

data Direction = Left | Right

data Kind = LetterL | LetterR | Number | Newline

%runElab derive "Kind" [Eq]

implementation TokenKind Kind where
  TokType LetterL = Direction
  TokType LetterR = Direction
  TokType Number = Integer
  TokType Newline = ?wja

  tokValue LetterL _ = Left
  tokValue LetterR _ = Right
  tokValue Number n = cast n
  tokValue Newline _ = ()

tmap : TokenMap (Token Kind)
tmap = [
  (exact "L", Tok LetterL),
  (exact "R", Tok LetterR),
  (digits, Tok Number),
  (newline, Tok Newline)
  ]

lineGrammar : Grammar state (Token Kind) True (Integer)
lineGrammar = do
  m <- choose (match LetterL) (match LetterR)
  n <- match Number
  pure $ either (const (n * (-1))) (const n) m


textGrammar : Grammar state (Token Kind) False (List Integer)
textGrammar = sepEndBy (match Newline) lineGrammar

export
sol : String -> String
sol = lex tmap
  ||> fst
  ||> parse textGrammar
  ||> either (const "error") (
    fst ||> foldl fn (50, 0) ||> snd ||> show
    )
  where
    fn : (Integer, Integer) -> Integer -> (Integer, Integer)
    fn (dial, zeroes) e = let
      d := mod (dial + e) 100
      in (d, zeroes + ( (d == 0) <|> (1,0) ))


export
sol2 : String -> String
sol2 = lex tmap
  ||> fst
  ||> parse textGrammar
  ||> either (const "error") (
    fst ||> foldl fn (50, 0) ||> snd ||> show
    )
  where
    fn : (Integer, Integer) -> Integer -> (Integer, Integer)
    fn (dial, zeroes) e = let
      d := mod (dial + e) 100
      u := abs $ div (dial + e) 100
      u := u + (((d == 0) && (e < 0)) <|> (1,0))
      u := u - (((dial == 0) && (e < 0) && (u > 0)) <|> (1, 0))
      in (d, zeroes + u)
