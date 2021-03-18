module Instances where

import Types

import Data.List (intersperse, concat)

{- Instances -} 

instance Show Type where
 show (Var v)   = v
 show (Const c) = c 
 show (Foo l r) = case l of 
   (Foo _ _) -> "( " ++ show l ++ " ) -> " ++ show r
   _         -> show l ++  " -> " ++ show r


instance Show a => Show (NestedList a) where
 show (Elem a) = show a 
 show (List xs) = " ( " ++ (concat . intersperse " -> " . map show $ xs) ++ " ) "


instance Foldable NestedList where
  foldr foo base (List (x:xs)) = foldr foo ( foldr foo base x ) (List xs)   
  foldr foo base (List [])     = base
  foldr foo base (Elem e)      = foo e base
