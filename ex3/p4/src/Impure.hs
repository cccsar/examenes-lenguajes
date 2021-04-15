module Impure (repl) where

import qualified Data.Map as M (member, insert)
import System.IO (hPutStrLn, stderr, stdout, hFlush) 
import System.Exit (exitSuccess)

import Core
import Constants
import Types


-- Main loop where user requests are served
repl :: Table -> IO () 
repl table = do
 putStrLn options
 resp <- askInput

 case words resp of  
   ("CLASS":name:":":parent:methods) -> addObjectAndParent table name parent methods >>= repl
   ("CLASS":name:methods)            -> addObject table name methods >>= repl 
   ("DESCRIBIR":name:[])             -> describeClass table name >> repl table
   ("SALIR":_)                       -> exitSuccess
   _                                 -> warn optionError >> repl table


-- Handles description of an existing class on the table
describeClass :: Table -> Class -> IO () 
describeClass table@(_,vtable) toDescribe 
 | M.member toDescribe vtable = case signatures of 
                                 [] -> putStrLn (prompt ++ "La clase no tiene metodos")
                                 _  -> mapM_ putStrLn prettyMethods
 | otherwise                  = warn noClassError
 where
  signatures = classDescription table toDescribe
  prettyMethods = map (\(method,tp) -> method ++ " -> " ++ tp ++ " :: " ++ method) signatures


-- Adds an object to the table
addObject :: Table -> Class -> [Method] -> IO Table
addObject table@(parents,vtable) newClass methods
 | cond && casing = return (parents, M.insert newClass methods vtable) 
 | otherwise      = warn invalidInputNoParent >> return table
 where 
  cond = not (M.member newClass vtable) && unique methods 
  casing = checkClass newClass && all checkMethod methods


-- Adds an object along with it's parent to the table
addObjectAndParent :: Table -> Class -> Class -> [Method] -> IO Table
addObjectAndParent table@(parents, vtable) new parent methods 
 | cond && casing = return (M.insert new parent parents, M.insert new methods vtable) 
 | otherwise      = warn invalidInputParent >> return table
 where 
  cond = not (M.member new vtable) && M.member parent vtable && unique methods
  casing = checkClass new && all checkMethod methods
  

{- Input helpers -} 

-- Request input using the prompt
askInput :: IO String
askInput = do 
  putPrompt
  hFlush stdout
  getLine
  

-- Display a nice prompt
putPrompt :: IO () 
putPrompt = putStr prompt


-- Handle standard warning messages
warn :: String -> IO () 
warn message = do
  putPrompt
  hPutStrLn stderr message
