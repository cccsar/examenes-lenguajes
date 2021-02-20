module Jaweno where

import Control.Parallel (pseq)

fact n = foldr (*) 1 [2..n] 
t n = drop (n-1) . take (n+1)
f = [0,1] ++ zipWith (+) f (tail f) 

foo :: Int -> Int
foo n = floor . log $ ( x/(y*z) )
 where
  [a,b] = t (n+1) f 
  x = fact b
  y = fact a
  z = fact (b-a) 


gg xs = foldr bar [] . zip xs $ (tail xs)  
 where 
  bar (b,c) xs@(a:cc)
   | b == c    = pseq (b:a) ((b:a):cc)
   | otherwise = pseq [b] ([b]:xs)
  bar (b,c) [] 
   | b == c    = [[c]]
   | otherwise = pseq [b] ([b] : [[c]] )
