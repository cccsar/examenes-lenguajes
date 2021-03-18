module Constants where

{- Regular messages -} 

prompt = "typo> " 

intro = "Bienvenido a Typo, simulador de tipos de datos" 

frequent = "Qué deseas hacer?\n"++
           "\tATOMICO <nombre> <n> <m>\tDefine el nombre <nombre> con n bytes de tamaño y alineación en m\n"++ 
           "\tSTRUCT <nombre> <tipos>\tDefine el struct nombre que contiene la lista de átomos <tipos>\n"++ 
           "\tUNION <nombre> <tipos>\tDefine la union <nombre> que contiene la lista de átomos <tipos>\n"++ 
           "\tDESCRIBIR <nombre>\tMuestra una descripción del espacio ocupado por nombre\n" ++ 
           "\tSALIR"


{- Warnings -} 

invalidOption = "La opción que ingreso no es una opcion vállida"

invalidAtomDescription = "La descripción del átomo es incorrecta. Deber ser:\n\tATOMO <nombre> <n> <m>"++
                         "\tcon n y m enteros y nombre un nombre aún no definido"

invalidCompoundFormat = "Debes dar un nombre no existente a tu estructura y colocar por lo menos un tipo."

invalidName = "El nombre a consultar no existe en memoria"
