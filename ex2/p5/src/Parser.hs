module Parser where

import Types

import Text.ParserCombinators.ReadP
import Data.Char
import Text.Read (readMaybe, readEither)

{-
 - CFG for a valid type annotated expression
 -
 - Exp   :: Row | Exp ' -> ' Exp
 - Arrow :: Arrow ' -> ' Arrow | Names
 - Parns :: '(' Arrow ' )' 
 - Names :: Var | Cons
 - Var   :: Upper Cons | Upper
 - Cons  :: Lower Snoc | Lower
 - Snoc  :: Alnum Snoc | Alnum
 -
 - Alnum :: Upper Alnnum | Lower Alnum | Else Alnum
 - Upper :: ## uppercase letters by extension
 - Lower :: ## lowercase letters by extension
 - Else  :: ## Alphanumeric characters not lowercase or uppercase
 -} 

upper , lower, alpha :: ReadP Char

upper = satisfy isUpper
lower = satisfy isLower
alpha = satisfy isAlpha

konst, var :: ReadP String

konst = lower *> many alpha
var   = upper *> many alpha
names = choice [ konst, var ] 

arrow :: ReadP (String, String) 
arrow = do 
         x <- names
         skipSpaces
         string "->"
         skipSpaces
         y <- names
         return (x,y)

polyType :: ReadP Type 
polyType = undefined

actVar, actKonst, actFoo :: ReadP Type

actVar   = Var <$> var
actKonst = Const <$> var
actFoo   = undefined
