--
-- Testing script (Martin Erwig  --  Jan 2003)
--

module TestScript where


-- import here whatever modules to test
--
-- import Expr
-- import TypeCheck


-- You can define additional functions to be tested here
-- 
sumThree x y z = x+y+z


-- scripting functions
-- 
-- use testU for unary functions
-- use testB for binary functions
-- use testT for ternary functions
-- 
-- Arguments for the testing functions:
--   name: a string that describes the function to be tested
--   f:    the function to be tested
--
-- NOTE: if the arguments are elements of a data type, you need
--       a "deriving Show" clause in the data type definition
--       (or a customized instance definition for the Show class)
--   
showArg x = if head s=='(' then s else '(':s++")" where s = show x
testFun name f x = putStrLn (name++" "++showArg x++" = "++show (f x))
testU name f = mapM (testFun name f)

testB name f = testU name (uncurry f)

testT name f = testU name (\(x,y,z)->f x y z)


-- test scripts
-- 
-- define your test cases as list of arguments for the function to be tested;
-- for binary/ternary functions use pairs/triples
-- 
-- testEval1 = [N 3,Plus (N 3) (N 5)]
-- testEval2 = [Neg (Neg (N 9))]
testMult  = [(3,4),(9,9)]
testSum   = [(1,2,3),(7,8,9)]

-- define the test results by applying testing functions to test cases;
-- you can, of course, provide test cases also directly or expand
-- them on the fly
--
-- checkEval = testU "eval" eval testEval1
checkMult = testB "built-in multiply" (*) (testMult++[(6,7)])
checkSum  = testT "The sumThree function" sumThree [(1,2,3),(7,8,9)]