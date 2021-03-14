module Parser (
 functionApp, 
 displayResults,
 runParser
) where

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
 - CFG for a valid type annotated expression
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
                first  <- lower
                remain <- word
                return (first : remain) 

varLiteral   = do 
                first  <- upper
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


{- Ojo # DISPOSABLE FROM HERE-} 

-- A parser for optional whitespace
ws :: ReadP ()
ws = do many blank  
        return ()

blank :: ReadP Char
blank = satisfy isSpace


{- Running tests -} 

runParser :: String -> ReadP a -> (a,String) 
runParser input parser = case readP_to_S parser input of
  (x:xs) -> x
  _      -> error "error while parsing"

tc = "a->b"
tc1 = "a ->b"
tc2 = "a -> b" 
tc3 = "a  ->  b"
tc4 = "A->b"
tc5 = "a->B"
tc6 = "A->B"
tc7 = "Hola -> Como"
tc8 = "a -> ( a -> b ) -> c " 
tc9 = "(a -> b->c) -> (a ->b) ->a->c"
tc10 = " ( a -> b -> c) -> d -> e " 
fumado = "(a -> (a -> b -> c) -> c) -> a -> b " 

testList :: [String] 
testList = [tc,tc1,tc2,tc3,tc4,tc5,tc6,tc7, tc8, tc9, fumado]

runTests :: ReadP a -> [(a,String)] 
runTests p = map (flip runParser p) testList

displayResults :: IO () 
displayResults = do 
 mapM_ putStrLn (zipWith (++) (map ("String "++) testList) (map (\a -> "\tParsed to: " ++ show a) (runTests functionApp) ) )
