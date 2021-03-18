module Main where

import Constants (intro, frequent, prompt, invalidOption) 
import Impure
import Types

import qualified Data.Map as M
import System.IO (stderr, stdout) 
import System.Exit (exitSuccess)


main :: IO ()
main = do 
        putStrLn intro 
        putStrLn frequent
        repl (M.empty)


repl :: Memory -> IO ()
repl memory = do 
       option <- requestInfo stdout 

       case words option of 
         ("ATOMICO":xs)   -> getAtomic memory xs >>= repl
         ("STRUCT":xs)    -> getStructOrUnion memory xs True  >>= repl
         ("UNION":xs)     -> getStructOrUnion memory xs False >>= repl
         ("DESCRIBIR":xs) -> describeType memory (unwords xs) >> repl memory
         ("SALIR":_)      -> exitSuccess
         _                -> warning stderr invalidOption >> repl memory
