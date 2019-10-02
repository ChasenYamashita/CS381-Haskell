-- Name: Chasen Yamashita

module SVG where
    
-- 
-- The definitions from this file can be used to
-- render the semantics of Mini Logo.
--
-- NOTE: you have to put the following import
-- statement into the Haskell module to use the
-- function ppLines.
--

import System.IO

import Data.Maybe
import Data.List

-- 
-- NOTE: The import statement must be the first 
-- statement in the file (after the module header).
--


-- defintion of semantic domains
-- 
type Line  = (Int,Int,Int,Int)
type Lines = [Line]


-- Pretty printing of lines:
-- write an svg file
--
ppLines :: Lines -> IO ()
ppLines ls = do h <- openFile "MiniLogo.svg" WriteMode
                hPutStr h (svgHdr++concatMap ppLine ls++svgFtr)
                hClose h

-- fixed size and maginifaction factor
-- (can be generalized easily)
--
factor=100
yMax=1100

svgHdr = "<?xml version=\"1.0\" standalone=\"no\"?>\n \
         \ <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n \
         \    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n \
         \ <svg width=\"12cm\" height=\"11cm\" viewBox=\"0 0 1200 1100\"\n \
         \    xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n \
         \ <title>Mini Logo Result Viewer</title>\n \
         \ <desc>A set of line segments</desc>\n \
         \ <rect x=\"10\" y=\"10\" width=\"1180\" height=\"1080\" \
         \       fill=\"none\" stroke=\"red\" /> "
svgFtr = "</svg>\n"
          
ppLine :: Line -> String
ppLine (x,y,x',y') = "<path d=\"M "++ppPos x y++" L "++ppPos x' y'++"\" "++
                     "stroke=\"blue\" stroke-width=\"5\" />\n"

ppPos :: Int -> Int -> String
ppPos x y = show (50+factor*x)++" "++show (yMax-50-factor*y)

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------

--PROBLEM 1

data Cmd = LD Int
         | ADD
         | MUL
         | DUP
         deriving Show

type Prog = [Cmd]
type Stack = [Int]

--The top of the stack is the int furthest left, first in the list.

type D = Stack -> Stack


--Execute one command in Prog list, recursively call sems
--semCmd :: Prog -> Stack -> Stack
sem :: Prog -> D
sem (x:xs) y = sem xs (semCmd x y) 
sem [] x = x

--semCmd :: Cmd -> Stack -> Stack
semCmd :: Cmd -> D
semCmd (LD a) [] =  [a]
semCmd (LD a) x = [a] ++ x

semCmd (ADD) (x:y:xs) = [x+y] ++ xs
semCmd (ADD) (x:[]) = error "Need at least two ints on the stack to add!"
semCmd (ADD) [] = error "Need at least two ints on the stack to add!"

semCmd (MUL) (x:y:xs) = [x*y] ++ xs
semCmd (MUL) (x:[]) = error "Need at least two ints on the stack to multiply!"
semCmd (MUL) [] = error "Need at least two ints on the stack to multiply!"

semCmd (DUP) (x:xs) = [x,x] ++ xs
semCmd (DUP) [] = error "Stack is empty!"
 
---PROBLEM 2a



 
---PROBLEM 2b

--data Maybe a = Just a
--             | Nothing

data Cmd2 = LD2 Int
          | ADD2
          | MUL2
          | DUP2
          | DEF String Prog2
          | CALL String
          deriving Show

type Macros = [(String,Prog2)]
type Prog2 = [Cmd2]
type Stack2 = [Int]

type D2 = (Stack2, Macros) -> (Stack2, Macros)

--Prog2 -> (stack2, Macros) ->
sem2 :: Prog2 -> D2
sem2 [] a = a
sem2 (x:xs) a = sem2 xs (semCmd2 x a)

--Cmd2 -> (Stack2, [(String,Prog2)]) 
--a command is added to or augments a stack, OR 
--if it is a call or def, it will access the list of string+prog tuples
--to call a program.
semCmd2 :: Cmd2 -> D2
semCmd2 (LD2 x) (xs, m) = ((x:xs), m)

semCmd2 (ADD2) (x:y:xs, m) = (([x+y] ++ xs), m)
semCmd2 (ADD2) (x:[], m) = error "Need at least two ints on the stack to add!"
semCmd2 (ADD2) ([], m) = error "Need at least two ints on the stack to add!"

semCmd2 (MUL2) (x:y:xs, m) = (([x*y] ++ xs), m)
semCmd2 (MUL2) (x:[], m) = error "Need at least two ints on the stack to multiply!"
semCmd2 (MUL2) ([], m) = error "Need at least two ints on the stack to multiply!"

semCmd2 (DUP2) (x:xs, m) = (([x,x] ++ xs), m)
semCmd2 (DUP2) ([], m) = error "Stack is empty!"

semCmd2 (DEF name prgm) (stck, macs) = (stck, (name,prgm):macs)
--If no name or empty program, macros is not changed.
--semCmd2 (DEF a []) x = x

--Use lookup to find the first matching string of the macro tuples
--do sem2 if match found. Otherwise throw error

semCmd2 (CALL name) (stck, macs) = case lookup name macs of
                                 Just h -> sem2 (h) (stck, macs)
                                 otherwise -> error "No macro found!"

--PROBLEM 3

data Com = Pen Mode
         | MoveTo Int Int
         | Seq Com Com 
         deriving Show

data Mode = Up | Down 
          deriving (Eq, Show)

type State = (Mode, Int, Int)
--type Line = (Int, Int, Int, Int)
--type Lines = [Line]


--Initial state is Up, pen at 0,0. 
--
sem' :: Com -> Lines
sem' x = snd (semS x (Up,0,0))


semS :: Com -> State -> (State, Lines)
semS (Pen m) (_, x, y) = ((m,x,y),[])
--Line drawn from a,b to x,y. Only if pen is down on "paper"
semS (MoveTo x y) (mod, a, b) 
                              | mod == Up = ((mod, x, y),[])
                              | mod == Down = ((mod, x, y),[(a,b,x,y)])

-- Needs to take two commands and string them together if they're both
-- commands that affect the line.
-- First part will be the state, then the final lines.

-- As (State, Lines) is the return for semS, we need the resulting state of 
-- command #1. the first fst returns the State of (State,Lines) to be then
-- used in command #2. The final State is the result of cmd #2's state.

--The Lines portion concatenates the returned Lines of the first command with
--the Lines of the second command. The second portion must also process any
--changes to the State by the first command before being added to the line,
--assuming command 2 must always be executed second after the first.

semS (Seq com1 com2) (a,b,c) = ((fst $ semS com2 $ fst $ semS com1 (a,b,c)), ((snd (semS com1 (a,b,c))) ++ (snd $ semS com2 $ fst $ semS com1 (a,b,c))))



