module Unification where

import Impl
import Types

import qualified Data.Map as M

type Substitution = [(Type, NestedList Type)]


-- Checks that properties for a function call represented as a NestedList hold, 
-- then call unifier function.
callUnifier :: NestedList Type -> Maybe (NestedList Type)
callUnifier inputExpression@(List xs) 
  | sz > 1    = checkHead inputExpression
  | sz == 1   = Just (head xs)
  | otherwise = Nothing
  where sz = length xs


{- For a checks the head of a List for another List. This pattern in the encoding 
 - produced represents function application whereas the list head is the function
 - and the tail are its parameters
 - E.g.
 -      + 1 2 .. is processed as:
 -      |----------------------------------- + ---------------------------------||-------- 1 --------||-------- 2 --------|
 -      List [ List [ Elem (Const "Int"), Elem (Const "Int"), Elem (Const "Int")], Elem (Const "Int"), Elem (Const "Int") ]
 -}
checkHead :: NestedList Type -> Maybe (NestedList Type)
checkHead toCheck@(List (x:xs)) = case x of
  (List (y:ys))  
    | not (null xs) -> matchArguments x (List xs) 
    | otherwise     -> Just toCheck
  _                 -> Just toCheck
checkHead _                     = error "checkHead: Unexpected condition\n\tCalled on Elem" 


{- 
 - Casos:
 - const   const    --> check match
 - const   typeVar  --> error
 - typeVar const    --> assign on left side (?)
 - typeVar typeVar  --> propapgate substitution (?)
 -}



-- Previous checking before starting matching types.
matchArguments :: NestedList Type -> NestedList Type -> Maybe (NestedList Type)
matchArguments a b = do 
                      xs <- checkHead a 
                      ys <- checkHead b
                      runSpeculation xs ys


-- Match the types of a function formal parameters with it's arguments using
-- speulation function across the top level of a nested list.
runSpeculation :: NestedList Type -> NestedList Type -> Maybe (NestedList Type)
runSpeculation (List (x:xs))   (List (y:ys)) = 
  do 
   spec <- speculation x y 

   if null spec then 
     runSpeculation (List xs) (List ys)
     else  runSpeculation (applyAllSubs (List xs) spec) (List ys)
runSpeculation remain@(List (x:xs)) (List []) =  Just remain
runSpeculation _                    _         = error "runSpeculation: Unexpected condition"


-- Search within an element within a nested list and if found, extend the nested
-- list.
searchAndExtend :: Eq a => (a, NestedList a) -> NestedList a -> NestedList a
searchAndExtend (name,replacement) (Elem e)
  | name == e = replacement
  | otherwise = Elem e
searchAndExtend substitution (List xs) = List ( map (searchAndExtend substitution) xs ) 


-- Perfom all substituions described by the Substitution list.
applyAllSubs :: NestedList Type -> Substitution ->  NestedList Type
applyAllSubs = foldr searchAndExtend


-- Comparation between function formal parameters and its provided arguments.
speculation :: NestedList Type -> NestedList Type -> Maybe Substitution
speculation (Elem (Const a)) (Elem (Const b)) 
  | a == b    = Just []
  | otherwise = Nothing
speculation (Elem (Const a)) _                = Nothing 
speculation (Elem (Var a))   (Elem (Const b)) = Just [  (Var a, Elem (Const b)) ]
speculation (Elem (Var a))   (Elem (Var b))   = error "Aqui" -- Just [ (Var a, Elem (Var b)) ] 
speculation (Elem (Var a))   (List xs)        = Just [ (Var a, List xs) ] 
speculation (List xs)        (List ys)        = error "alla" -- Puyao
speculation (List xs)        _                = Just []
