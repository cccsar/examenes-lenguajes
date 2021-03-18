module Constants where

{- Constant String Messages -}

prompt = "polyType> " 

intro = "PolyType\nSimulador de sistema de tipos" 

frequent = "Que deseas hacer ahora?\n\tDEF <nombre> <tipo>\tdefino un nombre <nombre> con tipo <tipo>" ++ 
           "\n\tTIPO <expresion>\tConsulta el tipo de la expresion dada" ++ 
           "\n\tMOSTRAR <tipo>\tMuestra el diccionario de tipos actual" ++ 
           "\n\tSALIR"


{- Warnings -} 

invalidInputWarning = "La opcion que ingreso no es una opcion valida."

invalidTypeDescriptionWarning = "La descripcion dada para el tipo no es correcta." 

parensExpressionWarning = "La expression dada no esta bien parentizada"

invalidIdentifierWarning = "La expresion dada contiene nombres que no han sido guardados en memoria"

invalidExpressionWarning = "La expresion dada es invalida" 
