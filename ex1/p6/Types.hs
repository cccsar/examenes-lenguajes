module Types ( 
 Expression (..) , 
 SomeOrd
) where

data Expression = Sum Expression Expression
                | Substraction Expression Expression
                | Product Expression Expression
                | Division Expression Expression 
                | Cons Int
              deriving Eq -- make show instance

type SomeOrd = [String] -- Represents a sequence of numbers and valid operators

instance Show Expression where
  show (Sum a b)  = show a ++ " + " ++ show b 
  show (Substraction a b)  = show a ++ " - " ++ show b 
  show (Product a b)  
   | isExpr a && isExpr b  = wrap a ++ " * " ++ wrap b 
   | isExpr a              = wrap a ++ " * " ++ show b 
   | isExpr b              = show a ++ " * " ++ wrap b 
   | otherwise             = show a ++ " * " ++ show b 
  show (Division a b) 
   | isExpr a && isExpr b  = wrap a ++ " / " ++ wrap b 
   | isExpr a              = wrap a ++ " / " ++ show b 
   | isExpr b              = show a ++ " / " ++ wrap b 
   | otherwise             = show a ++ " / " ++ show b 
  show (Cons e)  = show e 
        

-- Surrounds a Show instance with parenthesis.
wrap :: Show a => a ->  String
wrap ss = "( "++show ss++" )"


-- Checks if an Expr is an addition or substraction.
isExpr :: Expression -> Bool 
isExpr exp = case exp of
  (Sum _ _) -> True
  (Substraction _ _) -> True
  _                  -> False
