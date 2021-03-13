module Types where

import qualified Data.Map as M

data Type = Var VarName 
          | Const ConstName
          | Foo Type Type -- this needs extension
        deriving Eq  

type VarName = String
type ConstName = String

type Atom = String
type Expression = [Atom] -- They'll be used for unification
type Association = M.Map Atom Type 


{- Instances -} 

instance Show Type where
 show (Var v)   = v ++ " "
 show (Const c) = c ++ " "
 show (Foo l r) = case l of 
   (Foo _ _) -> "( " ++ show l ++ ") -> " ++ show r
   _         -> show l ++ "-> " ++ show r
