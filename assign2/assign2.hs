--Name: Chasen Yamashita
--Assignment 2

module HW2Types where

-- Exercise 1a

type Numb = Int
type Name = [Char]
data Mode = Up | Down deriving Show
data Pos = NumPos Numb
         | RefPos Name 
         deriving Show

data Pars = Param Name
          | MultPars Name Pars
          deriving Show

data Vals = Values Numb Vals
          | OneVal Numb
          deriving Show

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def Name [Pars] Cmd
         | Call Name [Vals]
         | Multi Cmd Cmd 
         deriving Show


--Exercise 1b

vector = Def "vector" [Param "x1", Param "y1", Param "x2", Param "y2"] 
    $ Multi (Pen Up) 
    $ (Multi (Moveto (RefPos "x1", RefPos "y1")) 
    $ Multi (Pen Down) (Moveto (RefPos "x2", RefPos "y2")))



--Exercise 1c

stepping = Def "stepOnce" [Param "x1", Param "y1"] 
   $ Multi (Pen Down) 
   $ (Multi (Moveto (RefPos "x1", RefPos "y1+1")) (Moveto (RefPos "x1 + 1", RefPos "y1")))

someX = 3
someY = 5

steps :: Int -> Cmd
steps 0 = Pen Up;
steps a = Call "stepOnce" [Values (someX) $ OneVal (someY)]

-------------------------------


--Exercise 2a

--type Numb = Int
--type Name = [Char]

--circuit ::= gates links
--gates ::= num:gateFn ; gates | ϵ
--gateFn ::= and | or | xor | not
--links ::= from numb.numb to numb.numb; links | ϵ

data LinkText = From | To deriving Show

data GateFn = And | Or | XOR | Not
            deriving Show
data Links = Linkin LinkText Numb Numb LinkText Numb Numb Links
           | NoLink
           deriving Show
data Gates = GateVal Numb GateFn Gates 
           | NoGate
           deriving Show

data Circuit = Circ Gates Links
             deriving Show


--Exercise 2b

halfAdder :: Circuit
halfAdder = Circ (GateVal 1 XOR $ GateVal 2 And $ NoGate) 
     $ Linkin From 1 1 To 1 2 $ Linkin From 1 2 To 2 2 $ NoLink 


--Exercise 2c

prettyGatesFn :: GateFn -> String
prettyGatesFn And = "and"
prettyGatesFn Or = "or"
prettyGatesFn XOR = "XOR"
prettyGatesFn Not = "not"
 
prettyGates :: Gates -> String
prettyGates NoGate = ""
prettyGates (GateVal b c d) = show b ++ ":" ++ prettyGatesFn c ++ "\n" ++ prettyGates d
 
prettyLinks :: Links -> String
prettyLinks NoLink = ""
prettyLinks (Linkin fromTxt a b toTxt c d morelinks) = show fromTxt ++ show a ++ "." ++ show b ++ show toTxt ++ show c ++ "." ++ show d ++ "\n" ++ prettyLinks morelinks

prettyCircuit :: Circuit -> String
prettyCircuit (Circ gates links) = prettyGates gates ++ prettyLinks links
------------------------------------------------------

--Exercise 3a

data Expre = N Int
           | Plus Expre Expre
           | Times Expre Expre
           | Neg Expre

--Expre Expre Expre 
--Neg (Expre) Times N 7
--Neg (Plus N 3 N 4) Times N 7
--Times (Neg N 7) (Plus N 3 N 4) 

data Op = Add | Multiply | Negate deriving Show
data Exp = Number Int
         | Apply Op [Exp]
         deriving Show

--Therefore 
--Exp 
--Apply Multiply [Number 7, Exp]
--Apply Multiply [Number 7, Apply Negate [Exp]]
--Apply Multiply [Number 7, Apply Negate [Apply Add [Number 3, Number 4]
y = Apply Multiply [Number 7, Apply Negate [Apply Add [Number 3, Number 4]]]

--Exercise 3b

--The first representation relies on only one data type, which can represent either a number or an operation. 
--It's easier to derive equations from, but equations are mostly going to be derived from long strings of Expre.
--The syntax tree of Alternative A will also be easier to navigate, as each Expre will make up their own contained branch for their operations.

-- Alternative B uses two data types, and is harder to derive from. 
--Its trees will also have much longer roots, extending outward more recursively, and are easier to navigate. 
--However, the operations are structured unlike A, where one does not need two expre for Times and Add. The multiple data types allow for Exp to be less cluttered, and have a more abstract function (to do an operation or be a number).

--Exercise 3c

translate :: Expre -> Exp
translate (N x) = Number x 

translate (Neg (N x)) = Apply Negate [Number x]
translate (Neg (Neg (N a))) = Number a
translate (Neg (Plus (N x) (N y))) = Apply Negate [Apply Add [Number x, Number y]]
translate (Neg (Times (N x) (N y))) = Apply Negate [Apply Multiply [Number x, Number y]]

--Basis for Plus
translate (Plus (N x) (N y)) = Apply Add [Number x, Number y] 

translate (Plus (Plus (N y) (N z)) (N x)) = Apply Add [Number y, Number z, Number x] 
translate (Plus (N x) (Plus (N y) (N z))) = Apply Add [Number x, Number y, Number z] 
translate (Plus (Plus (N a) (N b)) (Plus (N y) (N z))) = Apply Add [Number a, Number b, Number y, Number z] 

-- Plus Mixed Grammar
translate (Plus (N x) (Times (N y) (N z))) = Apply Add [Number x, Apply Multiply [Number y, Number z]] 
translate (Plus (N x) (Neg (N y))) = Apply Add [Number x, Apply Negate [Number y]] 

translate (Plus (Times (N y) (N z)) (N x)) = Apply Add [Apply Multiply [Number y, Number z], Number x] 
translate (Plus (Neg (N y)) (N x)) = Apply Add [Apply Negate [Number y], Number x] 

--Basis for Times
translate (Times (N x) (N y)) = Apply Multiply [Number x, Number y]

translate (Times (Times (N y) (N z)) (N x)) = Apply Multiply [Number y, Number z, Number x]
translate (Times (N x) (Times (N y) (N z))) = Apply Multiply [Number x, Number y, Number z]
translate (Times (Times (N a) (N b)) (Times (N y) (N z))) = Apply Multiply [Number a, Number b, Number y, Number z]

--Mixed Times Grammar
translate (Times (N x) (Plus (N y) (N z))) = Apply Multiply [Number x, Apply Add [Number y, Number z]]
translate (Times (N x) (Neg (N y))) = Apply Multiply [Number x, Apply Negate [Number y]]

translate (Times (Plus (N x) (N y)) (N z)) = Apply Multiply [Apply Add [Number x, Number y], Number z]
translate (Times (Plus (N a) (N b)) (Times (N x) (N y))) = Apply Multiply [Apply Add [Number a, Number b], Number x, Number y]
translate (Times (Plus (N x) (N y)) (Neg (N a))) = Apply Multiply [Apply Add [Number x, Number y], Apply Negate [Number a]]

translate (Times (Times (N a) (N b)) (Plus (N c) (N d))) = Apply Multiply [Number a, Number b,Apply Add [Number c, Number d]]
translate (Times (Times (N a) (N b)) (Neg (N c))) = Apply Multiply [Number a, Number b, Apply Negate [Number c]]

translate (Times (Neg (N a)) (N b)) = Apply Multiply [Apply Negate [Number a], Number b]
translate (Times (Neg (N a)) (Plus (N b) (N c))) = Apply Multiply [Apply Negate [Number a], Apply Add [Number b, Number c]]
translate (Times (Neg (N a)) (Times (N b) (N c))) = Apply Multiply [Apply Negate [Number a], Apply Multiply [Number b, Number c]]
translate (Times (Neg (N a)) (Neg (N b))) = Apply Multiply [Apply Negate [Number a], Apply Negate [Number b]]