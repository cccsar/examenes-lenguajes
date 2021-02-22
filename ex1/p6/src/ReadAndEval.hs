module ReadAndEval (
 evalPre, 
 evalPost, 
 readPre, 
 readPost,
 eval
)
 where

import Types

import Data.Char (isDigit) 


{- Evaluating prefix and infix expressions.-}


evalPre, evalPost :: SomeOrd -> Maybe Int -- Perhaps use either to handle error types

evalPre  ss = readPre ss >>= eval 
evalPost ss = readPost ss >>= eval 


{- Transfroming a post-order or pre-order expression into an infix one. -} 


readPre, readPost :: SomeOrd -> Maybe Expression


-- Receive a list of strings representing (possibly) a pre order expression, and 
-- return an expresion tree.
readPre = rpre [] . reverse 
 where
  -- rpre :: [Expression] -> SomeOrd -> Maybe Expression
  rpre xs (s:ss)
   | isNumber s   = rpre ( Cons (read s) : xs) ss
   | isOperator s = case xs of 
       (a:b:zs) ->  rpre ( getExp (head s) a b : zs ) ss
       _        -> Nothing
   | otherwise    = Nothing
  rpre [r] []     = Just r 
  rpre _   _      = Nothing


-- Receive a list of strings representing (possibly) a post order expression, and
-- return an expression tree.
readPost = rpos [] 
 where
  -- rpost :: [Expression] -> SomeOrd -> Maybe Expression
  rpos xs (s:ss) 
   | isNumber s   = rpos ( Cons (read s) : xs) ss 
   | isOperator s = case xs of 
       (a:b:zs) -> rpos ( getExp (head s) b a : zs) ss
       _        -> Nothing
   | otherwise    = Nothing
  rpos [r] []     = Just r 
  rpos _   _      = Nothing
  

-- Reduce an expression to its arithmetical result.
eval :: Expression -> Maybe Int
eval (Sum a b)          = fmap (+) (eval a) <*> (eval b)
eval (Substraction a b) = fmap (-) (eval a) <*> (eval b)
eval (Product a b)      = fmap (*) (eval a) <*> (eval b) 
eval (Division a b)     
  | b == (Cons 0)       = Nothing -- Consider division by zero
  | otherwise           = fmap div (eval a) <*> (eval b) 
eval (Cons a)           = Just a 


{- Helper Functions -}

-- Checks if a string is a number
isNumber :: String -> Bool
isNumber = foldr (\b acc-> isDigit b && acc) True

-- checks if a string contains an operator
isOperator :: String -> Bool
isOperator [e] = elem e ['+','-','*','/'] 
isOperator _   = False

getExp :: Char -> Expression -> Expression -> Expression   
getExp = fromJust . (flip lookup expMap)

expMap = [('+',Sum),('-',Substraction),('*',Product),('/',Division)]

fromJust (Just e) = e
