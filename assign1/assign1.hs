
module HW1types where
import Data.List (nub,sort,isInfixOf)

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
type Bag a = [(a,Int)]

norm :: Ord a => [a] -> [a]
norm = sort . nub

----------------------
somethin = []
-----------
compareFirst :: Eq a => a -> Bag a -> Bool
compareFirst a [] = False
compareFirst a [(b,c)]
    | (a == b) = True
    | otherwise = False

compareBagsEqual :: Eq a => Bag a -> Bag a -> Bool
compareBagsEqual [] [] = True
compareBagsEqual a [] = False
compareBagsEqual [] a = False
compareBagsEqual [(a,b)] ([(c,d)])
    | (a == c) = True
    | otherwise = False

compareOccur :: Eq a => Bag a -> Bag a -> Bool
compareOccur [] [] = True
compareOccur a [] = False
compareOccur [] a = False
compareOccur [(a,b)] [(c,d)]
    | (b <= d) = True
    | otherwise = False

getOccur :: Bag a -> Int
getOccur [] = 0
getOccur [(a,b)] = b

getNumb :: Bag a -> a
getNumb [(a,b)] = a

occurIncrease :: Bag a -> Bag a
occurIncrease [] = []
occurIncrease [(a, b)] = [(a, b+1)]

occurDecrease :: Bag a -> Bag a
occurDecrease [] = []
occurDecrease [(a, b)] 
    | (b - 1 == 0) = []
    | otherwise = [(a, b-1)]

-----------------------

-- Exercise 1a 

ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a,1)]
ins x (y:ys) 
    | compareFirst x [y] == True = occurIncrease [y] ++ (ins x ys)
    | otherwise = y : (ins x ys)   

-- Exercise 1b

del :: Eq a => a -> Bag a -> Bag a
del _ [] = []
del x (y:ys) 
    | compareFirst x [y] == True = occurDecrease [y] ++ (del x ys)
    | otherwise = y : (del x ys)

-- Exercise 1c   (Not Finished)

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = (ins x somethin) ++ (bag xs)

-- could use map or $ to apply ins to the whole list?

-- Exercise 1d  (Not Finished)

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] [] = True
--subbag (x:xs) (y:ys) 
--    | ((compareBagsEqual x y == True) && (compareOccur x y == True)) = occurDecrease [y] ++ (subbag x ys)
--    | otherwise = False

--Remember, a Bag is [(a,Int)]

-- Exercise 1e (Not Finished)

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag = istBy (==)

istBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
istBy _ [] _ = []
istBy _  _  [] = []
istBy eq xs ys = [x | x <- xs, any (eq x) ys]

-- Exercise 1f

size :: Bag a -> Int
size [] = 0
size (x:xs) = (getOccur [x]) + (size xs)

-- Exercise 2


-- Remember,
-- Node = Int
-- Edge = (Int, Int)
-- Graph = [(Int, Int)]
-- Path = [Int]

--g :: Graph
--g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
--h :: Graph
--h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

--Exercise 2a

getNodeA :: Edge -> [Node]
getNodeA (a,b) = [a]

getNodeB :: Edge -> [Node]
getNodeB (a,b) = [b]

nodes :: Graph -> [Node]   -- [(Int, Int)] -> [Int]
nodes [] = []
nodes (x:xs) = sort( nub (getNodeA x ++ getNodeB x ++ nodes xs))


--Exercise 2b
suc :: Node -> Graph -> [Node]
suc _ [] = []
suc a (x:xs) 
    | (getNodeA x == [a]) = (getNodeB x ++ suc a xs)
    | otherwise = suc a xs

--Exercise 2c

detach :: Node -> Graph -> Graph
detach _ [] = []
detach a (x:xs) 
    | (getNodeA x == [a] || getNodeB x == [a]) = [] ++ detach a xs
    | otherwise = [x] ++ detach a xs

-- Exercise 2d
makeEdge :: Node -> Node -> Graph
makeEdge a b = [(a,b)]

cyc :: Node -> Graph
cyc a
    | (a == 1) = []
    | otherwise = (cycRec a) ++ (makeEdge a 1)

cycRec :: Node -> Graph
cycRec a 
    | (a == 1) = []
    | otherwise = (cycRec (a-1)) ++ (makeEdge (a-1) a)

-- Exercise 3

type Number = Int
type Point = (Number,Number)
type Length = Number   -- = Int

data Shape = Pt Point
            | Circle Point Length
            | Rect Point Length Length
            deriving Show

type Figure = [Shape]
type BBox = (Point,Point)



--Exercise 3a

width :: Shape -> Length  -- Either (Int,Int) , (Int,Int) Int -> Int
width (Pt _) = 0
width (Circle _ a) = a * 2
width (Rect _ a _) = a

--Exercise 3b

--BBox is type (Point, Point) = ((Int,Int),(Int,Int))

bbox :: Shape -> BBox
bbox (Pt a) = (a,a)
bbox (Circle (x,y) a) = (((x-a),(y-a)), ((x+a),(y+a)))
bbox (Rect (x,y) a b) = ((x,y), ((x+a),(y+b)))

--Exercise 3c

minX :: Shape -> Number
minX (Pt (a,b)) = a
minX (Circle (x,y) c) = x-c
minX (Rect (x,y) a b) = x



--Exercise 3d

addPt :: Point -> Point -> Point
addPt (x,y) (a,b) = (x+a,y+b)

move :: Shape -> Point -> Shape
move (Pt x) a = Pt (addPt x a)
move (Circle x r) a = Circle (addPt x a) r
move (Rect x w h) a = Rect (addPt x a) w h 


--Exercise 3e

moveToX :: Number -> Shape -> Shape
moveToX a (Pt (x,y)) = Pt (a,y)
moveToX a (Circle (x,y) r) = Circle (a+r,y) r
moveToX a (Rect (x,y) w h) = Rect (a,y) w h 

alignLeft :: Figure -> Figure -- [Shape] -> [Shape]
alignLeft [] = []
alignLeft (x:xs) = [(moveToX (foldr1 min (map minX (x:xs))) x)] ++ (alignContinue (foldr1 min (map minX (x:xs))) xs)

alignContinue :: Number -> Figure -> Figure
alignContinue _ [] = []
alignContinue a (x:xs) = [(moveToX a x)] ++ (alignContinue a xs)



--Exercise 3f

inside :: Shape -> Shape -> Bool
-------------------------------------------------------
--Point inside a Shape
-------------------------------------------------------
inside (Pt a) (Pt b) = if (a == b) then True else False

inside (Pt (x,y)) (Circle (a,b) r)
    | (x < (a-r)) || (y < (b-r)) || (x > a+r) || (y > b+r) = False
    | ((x-a)^2 + (y-b)^2 > (r^2)) = False
    | otherwise = True

inside (Pt (x,y)) (Rect (a,b) w h) 
    | (x < a) || (y < b) = False
    | (x > (a+w)) || (y > (b+h)) = False
    | otherwise = True

-------------------------------------------------------
--Circle inside a Shape
-------------------------------------------------------
inside (Circle _ _) (Pt _) = False
inside (Circle (x,y) r) (Circle (a,b) h)
    | (r > h) = False
    | ((x-r) < (a-h)) || ((y-r) < (b-h)) = False 
    | ((x+r) > (a+h)) || ((y+r) > (b+h)) = False
    | (((x+r)-a)^2 + ((y+r)-b)^2 > (r^2)) = False
    | (((x+r)-a)^2 + ((y-r)-b)^2 > (r^2)) = False
    | (((x-r)-a)^2 + ((y+r)-b)^2 > (r^2)) = False
    | (((x-r)-a)^2 + ((y-r)-b)^2 > (r^2)) = False
    | otherwise = True     
-- safeguards against a circle lying within another's bounding box, but not their entire shape.

inside (Circle (x,y) r) (Rect (a,b) w h)
    | ((x-r) < a) || ((y-r) < b) = False 
    | ((x+r) > (a+w)) || ((y+r) > (b+h)) = False
    | otherwise = True


-------------------------------------------------------
--Rectangle inside a Shape
-------------------------------------------------------
inside (Rect _ _ _) (Pt _) = False

inside (Rect (x,y) w h) (Circle (a,b) r)
    | (x < (a-r)) || (y < (b-r)) || (x > a+r) || (y > b+r) = False
    | ((x-a)^2 + (y-b)^2 > (r^2)) = False
    | (((x+w)-a)^2 + ((y+h)-b)^2 > (r^2)) = False
    | (((x+w)-a)^2 + ((y)-b)^2 > (r^2)) = False
    | (((x)-a)^2 + ((y)-b)^2 > (r^2)) = False
    | (((x)-a)^2 + ((y+h)-b)^2 > (r^2)) = False

inside (Rect (x,y) w h) (Rect (a,b) c d)
    | (x < a) || (y < b) = False
    | (x > a+c) || (y > a+d) = False
    | otherwise = True