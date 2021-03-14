module Main where

import Constants (intro, invalidInput, frequent)
import Impure
import Types

import qualified Data.Map as M (empty, insert, toList) 
import System.Exit
import System.IO (stdout,hFlush,stderr,hPutStrLn, Handle, hPutStr) 


main = putStrLn intro >> repl M.empty


repl :: Memory -> IO () 
repl mem = do
   putStrLn frequent
   toParse <- requestInfo stdout ""

   case words toParse of 
     ("DEF":name:tipo)   -> defineType mem name (unwords tipo) >>= repl
     ("TIPO":expression) -> undefined
     ["MOSTRAR"]         -> mapM_ putStrLn (map (\(a,b) -> show a ++ ": " ++ show b) (M.toList mem))
     ["SALIR"]           -> exitSuccess
     _                   -> warning stderr invalidInput

   repl mem
