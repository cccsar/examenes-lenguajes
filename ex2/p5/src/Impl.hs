module Impl where

import Types
import Parser

import Data.List (intersperse, concat)  -- only for visibility
import Data.List.Split (splitWhen) 

{- # Onlly for debugging -} 
instance Show a => Show (NestedList a) where
 show (Elem a)    = show a 
 show (List xs) = "[ " ++ (concat . intersperse " , " . map show $ xs) ++ " ]"


-- Given a Type, transform it into an appropriate representation for unification
-- using NestedList type.
typeToList :: Type -> [NestedList Type]
typeToList (Var a)   = [Elem (Var a) ]
typeToList (Const a) = [Elem (Const a) ]
typeToList (Foo l r) = case l of
   (Foo _ _) -> case r of
                 (Foo _ _) -> [ List (typeToList l) ] ++ typeToList r
                 xd        -> [ List (typeToList l), Elem xd ]
   xd        -> case r of 
                 (Foo _ _) -> [ Elem xd ] ++ typeToList r 
                 xs        -> [ Elem xd, Elem xs ] 


-- Get a nestedList of types from an input string.
typeList :: String -> [NestedList Type]
typeList = typeToList . read 


wellParens :: String -> Bool
wellParens expression
 | null parens = True
 | otherwise   = match [] parens
 where
  isParen e = e == ')' || e == '('
  parens = filter isParen expression

  match :: String -> String -> Bool 
  match stack (p:arens) 
   | p == ')' && null stack        = False
   | p == ')' && head stack == '(' = match (tail stack) arens
   | p == ')'                      = False
   | otherwise                     = match (p:stack) arens
  match []    []        = True
  match _     []        = False


-- Assuming well parenthesized (if so) String expression, transform a string
-- into an appropriate NestedList representation for unification.
expressionList :: String -> [Expression]
expressionList expression
  | length toWork == 1 = head toWork
  | otherwise          = arrange [List (head toWork)] (tail toWork) parens
 where
  isParen e = e == ')' || e == '(' 
  parens    = filter isParen expression
  toWork    = map (map Elem) . map words $ splitWhen isParen expression :: [[NestedList String]]

  -- arrange :: [NestedList String] -> [[NestedList String]] -> String -> NestedList String
  arrange outStack (top:inStack) (thisParen:other) 
    | thisParen == '(' = arrange (List top:outStack) inStack other
    | thisParen == ')' = let (x : List y  : xs) = outStack 
                        in arrange ( List (y ++ [x]++ top ) : xs ) inStack other 
  arrange outStack _             []                = outStack
