## Indicaciones de tests

Para probar el IO simplemente hacer
`` 
   cabal clean 
   cabal build
   cabal test
``

Para las pruebas de cobertura hacer:
``
  cabal clean
  cabal configure --enable-tests --enable-coverage
  cabal test
``

y revisar los html bajo: **./dist/hpc/vanilla/html/**
