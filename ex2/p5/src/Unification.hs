module Unification where

import Impl
import Types

import qualified Data.Map as M

type Substitution = [(Type, NestedList Type)]


-- Checks that properties for a function call represented as a NestedList hold, 
-- then call unifier function.
callUnifier :: NestedList Type -> Maybe (NestedList Type)
callUnifier inputExpression@(List xs) 
  | sz > 1    = matchArguments (head xs) (tail xs) 
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
    | not (null xs) -> matchArguments x xs 
    | otherwise     -> Just toCheck
  _                 -> Just toCheck
checkHead xs                     = Just xs --error "checkHead: Unexpected condition\n\tCalled on Elem" 


{- 
 - Casos:
 - const   const    --> check match
 - const   typeVar  --> error
 - typeVar const    --> assign on left side (?)
 - typeVar typeVar  --> propapgate substitution (?)
 -}



-- Previous checking before starting matching types.
matchArguments :: NestedList Type -> [NestedList Type] -> Maybe (NestedList Type)
matchArguments a (b:bs) = do 
                      xs <- checkHead a 
                      ys <- checkHead b
                      --Just ys
                      runSpeculation xs (List (ys:bs)) 
matchArguments _ []    = error "matchArguments: Unexpected condition" 


-- Match the types of a function formal parameters with it's arguments using
-- speulation function across the top level of a nested list.
runSpeculation :: NestedList Type -> NestedList Type -> Maybe (NestedList Type)
runSpeculation (List (x:xs)) (List (y:ys)) = 
  do 
   spec <- speculation x y 

--   case y of 
--     (List e) -> error ("Puta lista de mierda "++show (List (x:xs)) ++ "    " ++ show (List (y:ys))) 
--     _ -> do 

   a <- checkHead (List xs)
   b <- checkHead (List ys) 

   if null spec then 
      do runSpeculation a b  
      else  runSpeculation (applyAllSubs a spec) b 
runSpeculation remain (List [])            = case remain of 
                                               (List [])     -> error "Pilas"
                                               (List [x])    -> Just x
                                               (List (x:xs)) -> Just remain
runSpeculation xd                    _     = error "runSpeculation: Unexpected condition"


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
