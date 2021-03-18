module Types where

import qualified Data.Map as M (Map(..)) 

data Type = Var VarName 
          | Const ConstName
          | Foo Type Type 
        deriving Eq

data NestedList a = List [NestedList a] | Elem a deriving Eq

type VarName = String
type ConstName = String

type Atom = String
type Expression = NestedList Atom 
type Memory = M.Map Atom (NestedList Type) 
