module Input where

import Data.Char (isDigit)

isNumber :: String -> Bool
isNumber = all isDigit
