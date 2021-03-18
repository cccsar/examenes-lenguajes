module Parser ( functionApp ) where

import Types

import Text.ParserCombinators.ReadP (ReadP, satisfy, munch, choice, (<++),
                                     readP_to_S, skipSpaces, char, string, 
                                     between,  many) 
import Data.Char (isUpper, isLower, isAlpha, isSpace) 
import Text.Read (readMaybe, readEither) -- remains to be used


-- A way to propperly read Type's using a read instance
instance Read Type where
  readsPrec _ = readP_to_S functionApp

 
{-
 - Context Free Grammar for a valid type annotated expression
 -
 - functionApp :: Name ' -> ' functionApp | Pt ' -> ' functionApp | Name [ | Pt ] ??
 - Pt    :: '(' functionApp ' )' 
 - Name  :: varLiteral | Cons
 - varLiteral   :: Upper Cons | Upper
 - Cons  :: Lower Snoc | Lower
 - Snoc  :: Alnum Snoc | Alnum
 -
 - Alnum :: Upper Alnnum | Lower Alnum | Else Alnum
 - Upper :: ## uppercase letters by extension
 - Lower :: ## lowercase letters by extension
 - Else  :: ## Alphanumeric characters not lowercase or uppercase
 -} 


{- Smallest parser blocks to build from -} 

upper , lower :: ReadP Char

upper = satisfy isUpper
lower = satisfy isLower

wordChecker :: Char -> Bool
wordChecker c = isAlpha c && not (isSpace c) 


{- Basic string parsers for variables and constants, built from previous ones -} 

word, konstLiteral, varLiteral, nameLiteral :: ReadP String

word = munch wordChecker

konstLiteral = do 
                first  <- upper
                remain <- word
                return (first : remain) 

varLiteral   = do 
                first  <- lower
                remain <- word
                return (first : remain) 

nameLiteral = konstLiteral <++ varLiteral  


{- Parsers for the type "Type", built from previous ones -} 

konst, var, name,  beginWithName, beginWithParens, parenthesized, functionApp :: ReadP Type

konst = Const <$> konstLiteral
var   = Var <$> varLiteral
name  = konst <++ var

beginWithName   = do 
                  skipSpaces
                  givenName <- name 
                  skipSpaces
                  arrow
                  skipSpaces
                  remaining <- functionApp

                  return (Foo givenName remaining)

beginWithParens = do 
                  skipSpaces 
                  liftedName <- parenthesized
                  skipSpaces
                  arrow
                  skipSpaces
                  remaining <- functionApp

                  return (Foo liftedName remaining)

parenthesized = between opening closing functionApp 

-- Final parser for type expressions
functionApp = beginWithName <++ beginWithParens <++ name 


{- Parsers for constant identifiers -} 

opening, closing :: ReadP Char 

opening = char '(' <* skipSpaces
closing = skipSpaces *> char ')' 

arrow :: ReadP String
arrow = string "->"
