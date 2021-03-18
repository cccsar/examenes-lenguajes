module Impure where

import Constants
import Instances
import NestedList
import Types
import Unification

import qualified Data.Map as M (empty, insert, toList, lookup) 
import System.IO (stdout,hFlush,stderr,hPutStrLn, Handle, hPutStr) 
import Text.Read (readMaybe) 
import Data.Maybe (isJust, fromJust) 


{- Module to separate auxiliar IO from Main IO -} 

-- Reads a definition string and if valid updates memory state
defineType :: Memory -> Atom -> String -> IO Memory
defineType mem name tipo = case readMaybe tipo of 
   (Just t) -> do 
               putStrLn ("\tSe definió \'"++name++"\' con tipo: "++tipo)
               return (M.insert name (typeToList t) mem)
   Nothing  -> do 
                warning stderr invalidTypeDescriptionWarning
                return mem


-- Reads an Expression and if valid, lookup names on it within the memory dictionary
-- and unify on the go.
giveExprType :: Memory -> String -> IO Memory
giveExprType memory expression 
  | wellParens expression = do
       let expressionNL         = expressionList expression  :: NestedList String
           maybeExpressionTypes = mapNestedList (flip M.lookup memory)  expressionNL 
           checkExpression      = foldr (\b acc -> isJust b && acc) True maybeExpressionTypes
                                  
       if checkExpression then do
          let expressionTypes = extendNestedList (fromJust . flip M.lookup memory) expressionNL 
              typeOfExprs     = callUnifier expressionTypes
               
          case typeOfExprs of
             (Just exp) -> putStrLn ('\t':show exp)
             _          -> warning stderr invalidExpressionWarning 

           -- Debug
          return memory
       else warning stderr invalidIdentifierWarning >> return memory
  | otherwise             = warning stderr parensExpressionWarning >> return memory



{- Helpers for text layout -} 

-- Requests input using prompt string.
requestInfo :: Handle -> String -> IO String
requestInfo handle message = do 
                   hPutStr handle prompt  
                   hFlush stdout
                   getLine
                   

-- Warns/tells the user messages using prompt and the appropriate file descriptor.
warning :: Handle -> String -> IO () 
warning handle message = hPutStrLn handle ('\t' : message) 
