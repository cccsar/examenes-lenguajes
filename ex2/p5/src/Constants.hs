module Constants ( 
 prompt, 
 intro, 
 frequent,
 invalidInput, 
 invalidTypeDescription,
 invalidExpression
)
 where

{- Constant String Messages -}

prompt = "polyType>" 

intro = "PolyType\nSimulador de sistema de tipos" 

frequent = "Que deseas hacer ahora?\n\tDEF <nombre> <tipo>\tdefino un nombre <nombre> con tipo <tipo>" ++ 
           "\n\tTIPO <expresion>\tConsulta el tipo de la expresion dada" ++ 
           "\n\tMOSTRAR <tipo>\tMuestra el diccionario de tipos actual" ++ 
           "\n\tSALIR"

invalidInput = "La opcion que ingreso no es una opcion valida."

invalidTypeDescription = "La descripcion dada para el tipo no es correcta." 

invalidExpression = "La expression dada no esta bien parentizada"
