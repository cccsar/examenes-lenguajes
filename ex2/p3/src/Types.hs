module Types where

import qualified Data.Map as M (Map(..)) 

data TypeDescriptor = Atom Size Alignment
                    | Struct [String]
                    | Union [String]
                   deriving (Eq,Show)

data StorageType = Normal | Packaged | Reordered deriving (Show, Enum, Bounded) 

type Size = Int
type Alignment = Int
type Name = String

type Memory = M.Map Name TypeDescriptor
