module Solution

import Lib
import Lib.Vect
import Debug.Trace

import Data.Vect
import Data.String

import Text.PrettyPrint.Bernardy

import Derive.Prelude
%language ElabReflection

data Manifold = Start
              | Empty
              | Splitter
              | Tachyon

%runElab derive "Manifold" [Eq]

opts : LayoutOpts
opts = Opts 60

Show Manifold where
  show Start = "S"
  show Empty = " "
  show Splitter = "^"
  show Tachyon = "|"

Pretty Manifold where
  prettyPrec _ Start = "S"
  prettyPrec _ Empty = " "
  prettyPrec _ Splitter = "^"
  prettyPrec _ Tachyon = "|"

parseManifold : Char -> Maybe Manifold
parseManifold 'S' = Just Start
parseManifold '.' = Just Empty
parseManifold '^' = Just Splitter
parseManifold _ = Nothing

public export
Grid : Type
Grid = ( n ** m ** Vect n (Vect m Manifold))

parse : String -> Maybe Grid
parse s = let lines := lines s |> map unpack
              Just l := head' lines | _ => Nothing
              l := length l
              Just lines := traverse {t=List,f=Maybe} (toVect l) lines
                          | _ => Nothing
              Just grid := traverse {f=Maybe} (traverse parseManifold) lines
                         | _ => Nothing
           in Just (length grid ** l ** fromList grid )

replaceSByTachyon : Vect n (Vect m Manifold) -> Vect n (Vect m Manifold)
replaceSByTachyon = replaceMatWhen (== Start) Tachyon

ex = ".S.\n.^.\n..."
ex1 : Vect ? (Vect ? Manifold)
ex1 = [
  [Tachyon, Empty, Tachyon],
  [Splitter, Empty, Splitter],
  [Empty, Empty, Empty]
  ]
ex2 : Vect ? (Vect ? Manifold)
ex2 = [
  [Empty, Tachyon, Empty],
  [Empty, Tachyon, Empty],
  [Empty, Tachyon, Empty]
  ]
||| assuming (fin ...) is the coordinates of
||| a tachyon, try to move the tachyon down
||| if the case below is a splitter, then spawn
||| a tachyon below left and right
||| return the updated grid and if the tachyon was splitted
drop : {n,m : Nat } -> Vect n (Vect m Manifold) ->
       (Fin m, Fin n) -> (Vect n (Vect m Manifold), Bool)
drop g c =
  let Just d := matDown c | _ => (g, False)
   in case indexMat d g of
           Empty => (replaceMatAt d g Tachyon, False)
           Splitter => [ matDR c, matDL c ]
                    |> catMaybes
                    |> List.filter (\c => indexMat c g /= Tachyon)
                    |> foldl (\g',c' => replaceMatAt c' g' Tachyon) g
                    |> (\g' => (g', True))
           _ => (g, False)

||| drop all tachyon in 1 step
||| return the updated grid and the number of tachyon splitted
dropTachyon : {n,m : Nat} -> Vect n (Vect m Manifold) -> (Vect n (Vect m Manifold), Integer)
dropTachyon g = let cs := allCoord g
                       |> filter (\c => (indexMat c g) == Tachyon)
                    g := replaceMatWhen (== Tachyon) Empty g
                 in foldl (
                   \(g',n),c => case drop g' c of
                                     (g'', True)  => (g'', n + 1)
                                     (g'', False) => (g'', n)
                   ) (g,0) cs


||| from the initial list
||| repeat each step until
||| the manifold doesn't change
partial
full : {n,m : Nat} -> Vect n (Vect m Manifold) -> (Vect n (Vect m Manifold), Integer)
full g = let g := replaceSByTachyon g
          in go (g,0) where
            go : (Vect n (Vect m Manifold), Integer) -> (Vect n (Vect m Manifold), Integer)
            go (g, i) = let (g', s) := dropTachyon g
                         in if g == g'
                               then (g, i+s)
                               else go (g', i+s)

extract : Grid -> Integer
extract (_ ** _ ** g) = let (_, i) := full g in i

export
sol : String -> String
sol = parse ||> maybe "Parse Error" (extract ||> show)
 

export
sol2 : String -> String
sol2 _ = "IMPLEMENT ME"
