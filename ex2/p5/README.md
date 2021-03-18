## Instrucciones de ejecucion

Para probar IO: 

`cabal run`

Y se interactua con el programa principal.

## Adicional

Este paquete no incluye pruebas unitarias.

El tipo de datos ***Type*** Fue la primera aproximación al parseo de tipos pero en la práctica no se usa. Esto porque posteriormente se consideró más útil al tipo ***NestedList*** y se resolvió simplemente transformar al primero en el segundo. 

En general se reconocen tipos concretos, sin demasiados paréntesis o aplicación parcial, pero muchos casos no fueron considerados.
