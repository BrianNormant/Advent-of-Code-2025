module Lib.Tree

import Data.Tree
import Data.List

import Lib.List

export
||| get a list representing the branch indexed by indexes
indexBranch : List Nat -> Tree a -> Maybe (List a)
indexBranch [] t = Just [t.value]
indexBranch (n :: ns) t = do
  f <- indexMaybe n t.forest
  es <- indexBranch ns f
  pure (t.value :: es)

export
||| get the whole tree indexed
indexTree : List Nat -> Tree a -> Maybe (Tree a)
indexTree [] t = Just t
indexTree (n :: ns) t = do
  f <- indexMaybe n t.forest
  indexTree ns f

export
||| append new branch at a certain point in the tree
||| this will not change the relative order of the branches
appendBranch : List Nat -> Tree a -> Tree a -> Maybe (Tree a)
appendBranch [] b t = Just $ {forest $= (++ [b])} t
appendBranch (n :: ns) b (T _ []) = Nothing
appendBranch (n :: ns) b t = do
  old <- indexMaybe n t.forest
  new <- appendBranch ns b old
  frst <- updateMaybe n (const new) t.forest
  pure $ (T t.value frst)
