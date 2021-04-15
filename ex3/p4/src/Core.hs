module Core ( 
 checkClass, 
 checkMethod,
 unique,
 classDescription
) where

import Types

import Data.Char (isLower, isUpper) 
import Data.Maybe (fromJust)
import qualified Data.Map as M 
import qualified Data.Set as S

-- Guarantee a class is properly formatted
checkClass :: Class -> Bool
checkClass = isUpper . head  


-- Guarantee a class is properly formatted
checkMethod :: Method -> Bool
checkMethod = isLower . head


-- Checks uniqueness of all elements of a list
unique :: (Eq a, Ord a) => [a] -> Bool
unique xs = length xs == length (S.fromList xs)


-- Build from a Class an association list representing all methods available to it
classDescription :: Table -> Class -> Assoc
classDescription table root = foldl (extendDescription table) [] (ancestors table root)  


-- Increase the descritipon of the methods of a class by 1 ancentor's methods
extendDescription :: Table -> Assoc -> Class -> Assoc
extendDescription table@(_,vtable) accumSign current = finalSign ++ accumSign 
 where 
  accumMethods     = map fst accumSign
  currentMethods   = fromJust ( M.lookup current vtable ) 
  toIncludeMethods = currentMethods `listDiff` accumMethods 
  finalSign        = zip toIncludeMethods (repeat current)
  

-- Given a table and a class gather all classese ancestors to such class
ancestors :: Table -> Class -> [Class] 
ancestors table@(parents,_) current = 
 case M.lookup current parents of 
   Just el -> current : ancestors table el
   Nothing -> [current]


-- Return the difference list from the two input lists
listDiff:: Eq a => [a] -> [a] -> [a] 
listDiff xs ys = foldr (\b acc -> if elem b ys then acc else b : acc ) [] xs 
