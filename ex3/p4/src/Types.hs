module Types where

import qualified Data.Map as M ( Map (..) )

type Class      = String
type Method     = String
type VTable     = M.Map Class [Method] 
type ParentHood = M.Map Class Class
type Assoc      = [(Method,Class)] 

type Table      = (ParentHood, VTable)  
