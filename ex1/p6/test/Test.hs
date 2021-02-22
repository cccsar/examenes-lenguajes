module Main where

import ReadAndEval
import Types
import Test.HUnit

main = do runTestTT givenTests

givenTests = TestList [testEval1,testEval2,testRead1, testRead2] 

testEval1 = TestCase (assertEqual tt exp (evalPre . words $ inp))
 where
  tt = "evalPre . words \"+ * + 3 4 5 7\" should yield: Just 42 "
  inp = "+ * + 3 4 5 7"
  exp = Just 42

testEval2 = TestCase (assertEqual tt exp (evalPost . words $ inp) )
 where 
  tt = "evalPost . words \"8 3 - 8 4 4 + * +\" should yield: Just 69 "
  inp = "8 3 - 8 4 4 + * +"
  exp = Just 69

testRead1 = TestCase (assertEqual tt exp (readPre . words $ inp) )
 where
   tt = "readPre . words \"8 3 - 8 4 4 + * +\" should yield: (3 + 4) * 5 + 7"
   exp = (Just (Sum (Product (Sum (Cons 3) (Cons 4) ) (Cons 5)) (Cons 7)))
   inp = "+ * + 3 4 5 7"

testRead2 = TestCase (assertEqual tt exp (readPost . words $ inp ) )
 where
   tt = "readPost . words \"8 3 - 8 4 4 + * +\" should yield: 8 - 3 + 8 * (4 + 4) "
   exp = (Just (Sum (Substraction (Cons 8) (Cons 3)) (Product (Cons 8) (Sum (Cons 4) (Cons 4) ) ) ) )
   inp = "8 3 - 8 4 4 + * +"


extraTests = TestList [testDivByZ0, testDivByZ1, testDivByZ2, testDivByZ3, testInvalid0, testInvalid1, testInvalid2, testInvalid3] 


{- Division by zero -} 

-- Pre
testDivByZ0 = TestCase (assertEqual tt exp (readPre . words $ inp ) ) 
 where
  tt = "readPre . words $ \"0 0 /\" should yield: Nothing"
  exp = Nothing
  inp = "/ 0 0"

testDivByZ1 = TestCase (assertEqual tt exp (evalPre . words $ inp ) ) 
 where 
  tt = "evalPre . words $ \"/ 0 0\" should yield: Nothing" 
  exp = Nothing
  inp = "/ 0 0"


--Post
testDivByZ2  = TestCase (assertEqual tt exp (readPost . words $ inp ) ) 
 where 
  tt = "readPost . words $ \"0 0 /\" should yield: Nothing" 
  exp = Nothing
  inp = "0 0 /"


testDivByZ3 = TestCase (assertEqual tt exp (evalPost . words $ inp ) ) 
 where
  tt = "evalPost . words $ \"0 0 /\" should yield: Nothing"
  exp = Nothing
  inp = "0 0 /"



{- Invalid Characters (operator or number) -} 

--Pre
testInvalid0 = TestCase (assertEqual tt exp (readPre . words $ inp ) ) 
 where
  tt = "readPre . words $ \"= 2 3/\" should yield: Nothing"
  exp = Nothing
  inp = "= 0 0"

testInvalid1 = TestCase (assertEqual tt exp (evalPre . words $ inp ) ) 
 where 
  tt = "evalPre . words $ \"= 0 0\" should yield: Nothing" 
  exp = Nothing
  inp = "= 0 0"


--Post
testInvalid2 = TestCase (assertEqual tt exp (readPost . words $ inp ) ) 
 where
  tt = "readPost . words $ \"2 3 =/\" should yield: Nothing"
  exp = Nothing
  inp = "= 0 0"

testInvalid3 = TestCase (assertEqual tt exp (evalPost . words $ inp ) ) 
 where 
  tt = "evalPost . words $ \"0 0 =\" should yield: Nothing" 
  exp = Nothing
  inp = "0 0 ="
