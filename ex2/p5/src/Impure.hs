module Impure where

import Constants
import Impl
import Types
import Unification

import qualified Data.Map as M (empty, insert, toList) 
import System.IO (stdout,hFlush,stderr,hPutStrLn, Handle, hPutStr) 
import Text.Read (readMaybe) 


{- Module to separate auxiliar IO from Main IO -} 

-- Reads a definition string and if valid updates memory state
defineType :: Memory -> Atom -> String -> IO Memory
defineType mem name tipo = case readMaybe tipo of 
   (Just t) -> return (M.insert name (typeToList t) mem)
   Nothing  -> do 
                warning stderr invalidTypeDescription 
                return mem


-- Reads an Expression and if valid, lookup names on it within the memory dictionary
-- and unify on the go.
giveExprType :: Memory -> String -> IO () 
giveExprType memory expression 
  | wellParens expression = undefined
  | otherwise             = warning stderr invalidExpression



{- Helpers for text layout -} 

-- Requests input using prompt string.
requestInfo :: Handle -> String -> IO String
requestInfo handle message = do 
                   hPutStr handle (prompt ++ " " ++ message ++ ": ") 
                   hFlush stdout
                   getLine
                   

-- Warns/tells the user messages using prompt and the appropriate file descriptor.
warning :: Handle -> String -> IO () 
warning handle message = hPutStr handle (prompt ++ " " ++  message) 
