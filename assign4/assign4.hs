--Name: Chasen Yamashita

module Assign4 where

type Stack = [Int]
type Prog = [Cmd]

data Cmd = LD Int 
         | ADD 
         | MULT 
         | DUP
         | INC 
         | SWAP 
         | POP Int
         deriving Show

type Rank = Int 
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
-- Adds just an int to the stack, hence n = 0
-- "highest" rank, as it is the only cmd that can be used on an empty stack.
rankC (LD x) = (0, 1)
--Takes two ints from stack, returns 1. likewise with MULT.
rankC ADD = (2, 1)
rankC MULT = (2, 1)
--Takes an Int from stack and returns two of the int.
rankC DUP = (1, 2)
--Increments one int, and returns it to the top
rankC INC = (1, 1)
--Pops two elements from the top, adds the 2 of them in switched order.
rankC SWAP = (2, 2)
--Pops k elements from the top. Does not place any back on the stack.
rankC (POP k) = (k, 0)


--A rank error occurs in a stack program when an operation with rank (n; m) is executed on a stack with rank k < n. In other words, a rank error indicates a stack underflow... when there arent enough elements to fulfill arguments for a Cmd.


--Will compute the rank of a program.
rankP :: Prog -> Rank -> Maybe Rank
rankP [] x = Just x
rankP stac x = rank stac x

--Takes a list of commands. Each command has a rank found with rankC.
--To compute the overall rank of the program, each individual rank must be added.
rank :: Prog -> Rank -> Maybe Rank
rank [] ran = Just ran
--Given a rank and program, get the rankC of head Cmd x.
rank (x:xs) r =  let (n, m) = rankC x in
              if n > r then Nothing
              else rank xs ((r - n) + m)    
                -- new rank from rank of head Cmd x.

-- 1b)

--takes a program and a stack being worked with.
--If rank of program with stack creates a Nothing (error),
--return nothing.
semStatTC :: Prog -> Stack -> Maybe Stack
semStatTC p stac | (rankP p $ length stac) == Nothing = Nothing 
                 | otherwise = (semTypeCheck p stac)
            
--after verifying with RankP, operate sem for each program command.
semTypeCheck :: Prog -> Stack -> Maybe Stack
semTypeCheck [] stac = Just stac
semTypeCheck (x:xs) stac = sem x stac >>= semTypeCheck xs


--performs the functions for each Command.
sem :: Cmd -> Stack -> Maybe Stack
sem (LD x) xs = Just (x:xs)

sem (ADD) (x:y:s) = Just ((x+y):s)
sem (MULT) (x:y:s) = Just ((x - y):s)
sem (DUP) (x:xs) = Just (x:x:xs)
sem (INC) (x:xs) = Just (x + 1:xs)
sem (SWAP) (x:y:xs) = Just (y:x:xs)
sem (POP x) st = Just ((drop x) st)
sem _ st = Nothing


---------------------------


--Problem 2

--Problem 2

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type Pixel = (Int,Int)
type Image = [Pixel]

type BBox = (Int,Int)

-------------------------

semShape :: Shape -> Image 
semShape X = [(1,1)]
semShape (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- semShape s2] 
                 where d1 = semShape s1
semShape (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- semShape s1] 
                 where d2 = semShape s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

----------------------------------

bbox :: Shape -> BBox
bbox X = (1,1)
--TD a b puts a on top of b
bbox (TD a b) 
            | ax < bx = (bx, ay+by)  --a's x is less than b's x, bbox goes at b
            | ax >= bx = (ax, ay+by)  --otherwise bring b to a's location
    where (ax, ay) = bbox a
          (bx, by) = bbox b
--LR a b puts a to left of b
bbox (LR a b)
            | ay < by = (ax+bx, by)  --bounding box is ax and bx width, and height of 
            | ay >= by = (ax+bx, ay)  --whatever is higher placed
    where (ax, ay) = bbox a
          (bx, by) = bbox b

--s1 = LR (TD X X) X
--s2 = LR (TD X X) (TD X X)
--s3 = LR (LR X X) (LR X X)

checkrect :: Image -> Bool
checkrect img 
    | (maxy(img) * maxx(img) == length(img)) = False
    | otherwise = True

rect :: Shape -> Maybe BBox
rect x = if checkrect(semShape(x)) == False 
         then Just (maxx(semShape(x)), maxy(semShape(x)))
         else Nothing
 

--Problem 3a

--http://web.engr.oregonstate.edu/~erwig/cs381/homework/types.pdf

------(1) and (2)
--null :: [a] -> Bool

f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]
--g [] y = []

--f :: [x] -> y -> [x]. It will return a list if true ([y]), or original list x. 
  -- x should be a list of elements, and y could be any type.
--g :: [x] -> y -> [y] (or [])
  --regardless of whether x is empty or contains elements, either an empty list or [y] will be returned. The type of x is not irrelevant.

------(3)

   --The type of g is more general, as y is ignored and the output will be the same
   --no matter what list of elements x is. x can also be an empty list, with no specified type.
   
   --In f, f [3] "a" is not an acceptable input, because of Haskell's static typing. Therefore it is more specific than g.
   
------(4)

   --f and g have different types because x and y must be the same type in f. In g, y can be any type, whereas in f, x and y's type are dependent on each other. If y in f is specified as a Char, x must also be a char, meaning the output will be [Char].
   


--Problem 3b

--h :: [Int] -> [(a, Int)] -> [Int]
--h [x] [(a,b)] = [x] ++ [b]
h [x] [(a,b)] = [x + snd(b)]

--type of a doesn't matter.


--Problem 3c

--k :: (a -> b) -> ((a -> b) -> a) -> b

auxOne a = (a*2)  -- a -> b
--auxTwo takes an auxOne and returns a single element 

k f1 f2 = f1 (f2 (f1))  
-- first layer of f1 takes one element as an argument.
--second layer of f2 takes a function and returns some a.
-- the first layer of f1 will use the output of a from the third layer, passed on by f2, to produce b.

--k (auxOne) (auxOne ) = (auxOne a b) + (auxTwo((auxOne c d) e)) 

--Problem 3d

aOutputA :: a -> a
aOutputA a = a 

aOutputB :: a -> b
aOutputB = error("yeah no thanks")


   --This function seems impossible, in that a function cannot create a new "b" without any input besides function name and an input. 
   -- If one looks at succ, for example, something that takes one input and gives a 
   -- "new" output, its type is actually Enum a => a -> a. 
   --
   --The compiler can't infer what b will be because "a" doesn't have a type supplied to it. Any type -> any type wouldn't work in this sense: It's extremely broad, and polymorphic to the point of breaking the language.
       
