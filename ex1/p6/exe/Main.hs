module Main where

import Types 
import ReadAndEval
import Constants

import System.IO (hFlush, stdout, stderr, hPutStrLn) 
import System.Exit(exitSuccess)


main = do 

 putStrLn next
 mapM_ putStrLn (zipWith (++) (repeat " * ") options )
 putStrLn explanation

 request ""  >>= parse


parse :: String -> IO () 
parse ss = do 
 case words ss of  
  ("EVAL"   :"PRE" :xs) -> evalExpr True xs
  ("EVAL"   :"POST":xs) -> evalExpr False xs
  ("MOSTRAR":"PRE" :xs) -> showExpr True xs
  ("MOSTRAR":"POST":xs) -> showExpr False xs
  ["SALIR"]             -> exitSuccess
  _                     -> hPutStrLn stderr "Error: Invalid option" >> main

 whipeOut 

 main
 
{- Display of possible output -} 
evalExpr, showExpr :: Bool -> SomeOrd -> IO ()

evalExpr g exp
 | g         = checker (evalPre exp) 
 | otherwise = checker (evalPost exp) 

showExpr g exp 
  | g         = checker (readPre exp) 
  | otherwise = checker (readPost exp) 
     
checker :: Show a => Maybe a -> IO () 
checker exp = case exp of  
               Just e -> give (show e) 
               _      -> hPutStrLn stderr "Error. Invalid expression"
             


{- IO helpers -} 

-- Request a string inline
request :: String -> IO String
request ss = do 
  putStr (prompt ++ ss)
  hFlush stdout
  getLine

-- Give a string with prompt
give :: String -> IO ()
give ss = putStrLn (prompt ++ ss) 

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: (Int,Int) -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Wait for any input, clear the screen a reestablish position to 
-- left upper corner of screen.
whipeOut :: IO () 
whipeOut = getLine >> clearScreen >> goTo (0,0)
