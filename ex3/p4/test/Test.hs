module Main where

import Test.HUnit

import Core
import Types
import Constants

import qualified Data.Map as M (empty,fromList, Map(..))
import Data.List (sort)

main = do 
  -- tests for checkClass
  assertEqual "test1" (True) (checkClass "A") 
  assertEqual "test2" (False) (checkClass "a") 

  -- tests for checkMethod
  assertEqual "test3" (True) (checkMethod "f") 
  assertEqual "test4" (False) (checkMethod "F")  

  -- tests for unique
  assertEqual "test5" (True) (unique [1,2,3])
  assertEqual "test6" (False) (unique [1,1])

  let fSampleParenthood = M.fromList []
      fSampleVT         = M.fromList [("A",[])]
      a = (fSampleParenthood,fSampleVT) 

      sSampleParenthood = M.fromList [("B","A")] 
      sSampleVT         = M.fromList [("A",["f","g"]),("B",["f","h"])]
      b = (sSampleParenthood, sSampleVT)

      tSampleParenthood = M.fromList [("D","C"),("C","B"),("B","A")] 
      tSampleVT         = M.fromList [("D",["h","i"]),
                                      ("C",["h","i","j"]),
                                      ("B",["g","h","i","j"]),
                                      ("A",["f","g","h","i","j"])
                                     ]
      c = (tSampleParenthood,tSampleVT)

  -- tests for classDescription
  assertEqual "test7" [] (classDescription a "A") -- No method class
  assertEqual "test8" [("f","B"),("g","A"),("h","B")] (sort (classDescription b "B") )
  assertEqual "test9" [("f","A"),("g","B"),("h","D"),("i","D"),("j","C")] (sort (classDescription c "D")) 
