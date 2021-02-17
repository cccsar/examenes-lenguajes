module Constants (
 prompt, 
 next, 
 explanation, 
 options
) where

-- Constant strings used for interaction

{- Constants -} 

prompt, next, explanation :: String

prompt      = "Eval> "
next        = "What would you like to do?:"
explanation = "Where <order> is one of PRE or POST, and expr is an arithmeticall expression in such order\n"

options :: [String]
options = ["EVAL <order> <expr>", "MOSTRAR <order> <expr>", "SALIR"] 
