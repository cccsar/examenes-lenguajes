module Main where

import qualified Data.Map as M (empty)

import Constants (introduction)
import Impure

main = putStrLn introduction >> repl (M.empty, M.empty) 
