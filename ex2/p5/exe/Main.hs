module Main where

import Constants (intro, invalidInputWarning, frequent) 
import Impure
import Types

import qualified Data.Map as M (empty, insert, toList) 
import System.Exit
import System.IO (stdout,hFlush,stderr,hPutStrLn, Handle, hPutStr) 


main = do 
  putStrLn intro 
  putStrLn frequent
  repl M.empty


repl :: Memory -> IO () 
repl mem = do
   toParse <- requestInfo stdout ""

   case words toParse of 
     ("DEF":name:tipo)   -> defineType mem name (unwords tipo) >>= repl
     ("TIPO":expression) -> giveExprType mem (unwords expression) >>= repl
     ["MOSTRAR"]         -> mapM_ putStrLn (map (\(a,b) -> show a ++ ": " ++ show b) (M.toList mem))
     ["SALIR"]           -> exitSuccess 
     _                   -> warning stderr invalidInputWarning

   repl mem
