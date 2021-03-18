module NestedList where

import Types
import Parser

import Data.List.Split (splitWhen) 




{- Related Nested List functions -} 

-- Similar to list concatenation, but with nested Lists
concatNestedList :: NestedList a -> NestedList a -> NestedList a
concatNestedList (List xs) (List ys) = List (xs ++ ys) 
concatNestedList (List xs) (Elem e)  = List (xs ++ [Elem e])
concatNestedList (Elem e)  (List xs) = List (Elem e:xs) 
concatNestedList a         b         = List [a,b]


-- Map over a nested list
mapNestedList :: (a -> b) -> NestedList a -> NestedList b
mapNestedList foo (List xs) = List (map (mapNestedList foo) xs) 
mapNestedList foo (Elem e)  = Elem (foo e) 


-- Extend a nested list of type a, by mapping it a function that draws nested list of
-- type b from type a
extendNestedList :: (a -> NestedList b) -> NestedList a -> NestedList b
extendNestedList f (Elem e)  = f e 
extendNestedList f (List xs) = List (map (extendNestedList f) xs)  


-- Given a Type, transform it into an appropriate representation for unification
-- using NestedList type.
typeToList :: Type -> NestedList Type
typeToList (Var a)   = Elem (Var a) 
typeToList (Const a) = Elem (Const a) 
typeToList (Foo l r) = case l of
   (Foo _ _) -> List [ typeToList l ] `concatNestedList` typeToList r 
   _         -> typeToList l `concatNestedList` typeToList r



-- Assuming well parenthesized (if so) String expression, transform a string
-- into an appropriate NestedList representation for unification.
expressionList :: String -> Expression
expressionList expression
  | length toWork == 1 = List $ head toWork
  | otherwise          = arrange [List (head toWork)] (tail toWork) parens
 where
  isParen e = e == ')' || e == '(' 
  parens    = filter isParen expression
  toWork    = map (map Elem) . map words $ splitWhen isParen expression :: [[NestedList String]]

  arrange :: [NestedList String] -> [[NestedList String]] -> String -> NestedList String
  arrange outStack (top:inStack) (thisParen:other) 
    | thisParen == '(' = arrange (List top:outStack) inStack other
    | otherwise        = let (x : List y  : xs) = outStack 
                         in arrange ( List (y ++ [x] ++ top ) : xs ) inStack other 
  arrange outStack _             []                = head outStack


-- Check a string's inner parentheses form a well parenthisation.
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
