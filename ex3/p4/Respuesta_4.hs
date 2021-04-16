import xd
import Prelude hiding(foldr,takeWhile)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr  _ e [] = e
foldr f e (x:xs) = f x $ foldr f e xs

takeWhile :: (a -> Bool) -> [a] -> [a] 
takeWhile p = foldr (\a r -> if p a then a : r else []) [] 

gen :: Int -> [Int]
gen n = n : gen (n+1)


{- Orden de evaluacion normal -} 

takeWhile (<=3) (gen 1) -> 

foldr (\a r -> if (a<=3) then a : r else []) [] (gen 1) -> 

foldr (\a r -> if (a<=3) then a : r else []) [] (1 : gen (1+1)) ->  

if (1<=3) then 1 : (foldr (\a r -> if (a<=3) then a : r else []) [] (gen (1+1)) ) else [] -> 

1 : foldr (\a r -> if (a<=3) then a : r else []) [] (gen (1+1))  -> 

1 : foldr (\a r -> if (a<=3) then a : r else []) [] (2 : gen (2+1)) -> 

1 : if (2<=3) then 2 : (foldr (\a r -> if (a<=3) then a : r else []) [] (gen (2+1)) ) else [] -> 

1 : 2 : foldr (\a r -> if (a<=3) then a : r else []) [] (gen (2+1))  -> 

1 : 2 : foldr (\a r -> if (a<=3) then a : r else []) [] (3 : gen (3+1))  -> 

1 : 2 : if (3<=3) then 3 : (foldr (\a r -> if (a<=3) then a : r else []) [] (gen (3+1)) ) else [] -> 

1 : 2 : 3 : foldr (\a r -> if (a<=3) then a : r else []) [] (gen (3+1))  -> 

1 : 2 : 3 : foldr (\a r -> if (a<=3) then a : r else []) [] (4 : gen (4+1)) -> 

1 : 2 : 3 : if (4<=3) then 4 : (foldr (\a r -> if (a<=3) then a : r else []) [] (gen (4+1)) ) else [] -> 

1 : 2 : 3 : [] 

1 : 2 : [3]

1 : [2,3]

[1,2,3]

{- Orden de evaluacion aplicativo -} 

takeWhile (<=3) (gen 1) -> 
takeWhile (<=3) (1 : gen (1+1)) -> 
takeWhile (<=3) (1 : 2 : gen (2+1)) -> 
... 
takeWhile (<=3) (1 : 2 : .. : n-1 : gen ( (n-1) + 1 ) ) -> -- gen es el unico argumento evaluable de takeWhile
                                                           -- y siempre que se llama construye una list y repite una llamada
                                                           -- a si mismo con un nuevo valor, lo que hace que siempre sea
                                                           -- posible evaluarlo como parte de un argumento y nunca se termine por 
                                                           -- llamar a takeWhile


data Arbol a = Hoja  | Rama a (Arbol a) (Arbol a) 

takeWhile :: (a -> Bool) -> Arbool a -> Arbol a 
takeWhile p = foldA (\a i d -> if p a then Rama a i d else Hoja) Hoja

genA :: Int -> Arbol Int
genA n = Rama n (genA (n+1)) (genA (n*2)) 


foldA :: (a -> b -> b -> b) -> b -> Arbol a -> b
foldA f (Rama a i d) e = f a (foldA f e i) (foldA f e d) 
foldA f Hoja         e = e

-- Se renombra para facilitar lectura
foo = (\a i d -> if (a<=3) then Rama a i d else Hoja)


{- Orden Normal de evaluacion / escogiendo primero la que expresion a evaluar mas a la izquierda -}


takeWhileA (<=3) (genA 1) -> 

foldA foo Hoja (genA 1) -> 

foldA foo Hoja (Rama 1 (genA (1+1)) (genA (1*2))) -> 

if (1<=3) then Rama 1 foldA foo Hoja (genA (1+1)) ) 
                      foldA foo Hoja (genA (1*2)) )
          else Hoja -> 

Rama 1 foldA foo Hoja (genA (1+1)) ) 
       foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 foldA foo Hoja (genA 2) ) 
       foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 foldA foo Hoja (Rama 2 (gen (2+1)) (gen (2*2)) ) 
       foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 (
        if (2<=3) then Rama a foldA foo Hoja (gen (2+1))) 
                              foldA foo Hoja (gen (2*2))) 
                 else Hoja
       )
       foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (foldA foo Hoja (gen (2+1))) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 (
        Rama 2 (foldA foo Hoja (gen 3))                            -- A
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 (
        Rama 2 (foldA foo Hoja (Rama 3 (gen (3+1)) (gen (3*2)) ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 

Rama 1 (
        Rama 2 (
                  if (3<=3) then Rama 3 (foldA foo Hoja (gen (3+1)))
                                        (foldA foo Hoja (gen (3*2))) 
                            else Hoja
               ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (
                  Rama 3 (foldA foo Hoja (gen (3+1)))
                         (foldA foo Hoja (gen (3*2))) 
               ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (
                  Rama 3 (foldA foo Hoja (gen 4))                          
                         (foldA foo Hoja (gen (3*2))) 
               ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (
                  Rama 3 (foldA foo Hoja (Rama 4 (gen (4+1)) (gen (4*2))))
                         (foldA foo Hoja (gen (3*2))) 
               ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (
                  Rama 3 (
                          if (4<=3) then Rama 4 (foldA foo Hoja (gen (4+1)))  
                                                (foldA foo Hoja (gen (4*2)))
                                    else Hoja
                         ) 
                         (foldA foo Hoja (gen (3*2))) 
               ) 
               (foldA foo Hoja (gen (2*2))) 
       )
       (foldA foo Hoja (genA (1*2)) ) -> 


Rama 1 (
        Rama 2 (
                Rama 3 (
                         Hoja                                         
                         (foldA foo Hoja (gen (3*2)))  -- imp1
                       ) 
               ) 
               (foldA foo Hoja (gen (2*2)))            -- imp2
       )
       (foldA foo Hoja (genA (1*2)) ) -> 

-- Al igual que 4, (3*2) no cumple (6<=3),  imp1 e imp2 resultan en hojas

Rama 1 (
        Rama 2 (
                Rama 3 )          -- | 
                         Hoja     -- A' 
                         Hoja     -- | 
                       )          -- |
               ) 
               Hoja 
       )
       (foldA foo Hoja (genA (1*2)) ) ->     -- F


-- Asi mismo, vemos que  F tiene el mismo patron de A, que termina en A'

Rama 1 (
        Rama 2 (
                Rama 3 (
                         Hoja                                     
                         Hoja 
                       ) 
               ) 
               Hoja 
       )
       Rama 3 (
               Hoja                                         
               Hoja 
              ) 

{- Orden de evaluacion normal / escogiendo como proximo a reducir el que este mas a la izquierda -} 


takeWhileA (<=3) (genA 1) -> 

takeWhile (<=3) (Rama 1 (genA (1+1)) (genA (1*2)))

takeWhile (<=3) (Rama 1 (genA 2) (genA (1*2)))

takeWhile (<=3) (Rama 1 (
                         Rama 2 (genA (2+1)) (genA (2*2)) 
                        ) 
                        (genA (1*2)))

takeWhile (<=3) (
                 Rama 1 (
                         Rama 2 (genA 3) (genA (2*2)) 
                        ) 
                        (genA (1*2))
                )

takeWhile (<=3) (
                 Rama 1 (
                         Rama 2 (
                                 Rama 3 (gen (3+1)) (gen (3*2))
                                )
                                (genA (2*2)) 
                        ) 
                        (genA (1*2))
                )

.. 


takeWhile (<=3) (
                 Rama 1 (
                         Rama 2 (
                                 Rama 3 (
                                         ...
                                            ( 
                                             Rama n (gen (n+1)) (gen (n*2)) 
                                            ) 
                                            gen (n*2) 
                                         ...
                                        ) 
                                        (gen (3*2))
                                )
                                (genA (2*2)) 
                        ) 
                        (genA (1*2))
                )

-- Como vemos la evaluacion sobre gen n continua para n -> inf , por lo que 
-- nunca se evalua el takeWhile y la evaluacion en realidad no termina
