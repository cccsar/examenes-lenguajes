module Constants where

prompt       = "vth> "  
introduction = "Bienvenido a vth, manejador de tablas de métodos virtuales"
options      = "Escoge una de las siguientes opciones:\n"++
               "\tCLASS <tipo> [<nombre>]\ttipo debe empezar por mayuscula y nombre por minuscula\n"++
               "\tDESCRIBIR <nombre>\tnombre debe corresponder a un tipo existente\n"++
               "\tSALIR\n"

optionError  = "Opcion invalidad. Debe solicitar una de:\n"++
               "\tCLASS <tipo> [<nombre>]\n"++
               "\tDESCRIBIR <nombre>\n"++
               "\tSALIR\n"

invalidInputNoParent = "Debe ingresar una clase no existente con metodos no repetidos"
invalidInputParent   = "Debe ingresar una clase hijo no existente con una clase padre existente y"++
                       " no deben haber metodos repetidos"
noClassError         = "La clase proporcionada no existe"
