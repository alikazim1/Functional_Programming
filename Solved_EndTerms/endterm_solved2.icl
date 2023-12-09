module endterm_solved2
import StdEnv


/*------------------------------------*/

/* 1. Distances - 10 points (-- 5 points, distance 5 points)
A tuple (Real,Real,Real) indicates a 3D point's x,y,z coordinates.
Define the (--) binary operator on (Real,Real,Real) such that 
tuple1 -- tuple2 indicates the distance between the 2 points:
The distance between two 3D points is defined as: 
(x1,y1,z1) - (x2,y2,z2) = ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)^(1/2) 

Using -- implement the distance function, such that, it finds the 
distances between a given point to each of the points of a given list
and returns them in a list. */
(--) :: (Real,Real,Real) (Real,Real,Real) -> Real
(--) a b = ((fst3 b - fst3 a )^2.0+ (snd3 b - snd3 a)^2.0 + ( thd3 b - thd3 a)^2.0)^0.5

//Start = (32.51,24.12,4.14) -- (23.1,4.14,12.234)

distance :: (Real,Real,Real) [(Real,Real,Real)] -> [Real]
distance tup lst = map (\x = tup -- x) lst
// [13.5107364714141,12.0174872581584,19.0824526725471,63.7981190945313]
//Start = distance (0.0,0.0,0.0) [] // []
//Start = distance (2.8,0.9,1.0) [(0.0,0.0,0.0),(-1.7,-1.6,-1.7)] 
// [3.10644491340181,5.81291665173345]

/*------------------------------------*/

/* 2. Vowels removed - 10 points (condition 5 points, removing 5 points)
Write a function that takes an array of arrays of strings, 
removes the vowels from each string and returns the 
array of arrays of strings with the vowels removed. */

vowelList = ['a','e','i','o','u','A','E','I','O','U']

ax :: String -> String 
ax str = toString[ x \\ x <-: str | not(isMember x vowelList)]

aux :: {String} -> {String}
aux str = {ax x \\ x<-: str }
//Start= aux "helloappleorange"
remove1 :: {{String}} -> {{String}}
remove1 arr = { aux x \\ x <-: arr }
//Start = aux {"hello","world"}

//Start = remove1 {{"hello","world"},{"good","morning"}} 
// {{"hll","wrld"},{"gd","mrnng"}}
//Start = removeVowels {{"HELLO","WORLD"},{"GOOD","EVENING"},{"hello","again!"}} 
// {{"HLL","WRLD"},{"GD","VNNG"},{"hll","gn!"}}
//Start = removeVowels {{"","o"},{"aA","oOo","AEIOUaeiou"}} // {{"",""},{"","",""}}

/*------------------------------------*/

/* 3. Find lines - 20 points - each function of 5 points
In a parallel world, ELTE has a bus, a tram and a metro system.
There are 7 lines that go between the faculties of the university.
The lines are numbered from 1 to 7. */

:: Transport = Bus | Tram | Metro 
:: Stop = IK | PPK | GTK | AJK | TOK | TTK | TATK | BGGYK

:: Line = {transport:: Transport, number:: Int, stops:: [Stop]}

Line1 = {transport = Bus, number = 1, stops = [IK, PPK, GTK, AJK, TOK, TTK, TATK, BGGYK]}
Line2 = {transport = Tram, number = 2, stops = [IK, TATK, BGGYK]}
Line3 = {transport = Metro, number = 3, stops = [IK, TTK, TATK, BGGYK]}
Line4 = {transport = Bus, number = 4, stops = [IK, PPK, GTK, AJK, TOK]}
Line5 = {transport = Tram, number = 5, stops = [IK, AJK, TOK, TTK, BGGYK]}
Line6 = {transport = Metro, number = 6, stops = [TOK, TTK, TATK, BGGYK]}
Line7 = {transport = Bus, number = 7, stops = [IK, PPK, TOK, TTK]}

Linelist = [Line1, Line2, Line3, Line4, Line5, Line6, Line7]

instance == Transport 
where 
	(==) Bus Bus = True
	(==) Tram Tram = True
	(==) Metro Metro = True
	(==) _ _ = False
		 
instance == Stop
where 
	(==) PPK PPK = True
	(==) IK IK = True
	(==) GTK GTK = True
	(==) AJK AJK = True
	(==) TOK TOK = True
	(==) TTK TTK = True
	(==) TATK TATK = True
	(==) BGGYK BGGYK = True
	(==) _ _ = False
			 
/*------------------------------------*/
//Start= [(line.transport,line.number)\\ line <- Linelist| isMember IK line.stops]

taux :: Transport -> [Stop]
taux tr  = flatten[ l.stops \\ l <- Linelist | l.transport == tr ]

/* 3.1. Write a function that for a faculty checks if it
is reachable by the given transport. */
FindLinesC :: Stop Transport -> Bool
FindLinesC st tr = isMember (st) (taux tr)

//Start = taux Bus 
//Start = flatten[[1,2],[3,4]]
//Start = FindLinesC IK Bus // True
//Start = FindLinesC PPK Metro // False
//Start = FindLinesC GTK Tram // False

/*------------------------------------*/

/* 3.2. Write a function that for a faculty finds the line and 
the transport pairs that are reaching it. */
FindLinesR :: Stop -> [(Int,Transport)]
FindLinesR st = [(x.number,x.transport)  \\ x <- Linelist | isMember st x.stops]

//Start = FindLinesR IK // [(1,Bus),(2,Tram),(3,Metro),(4,Bus),(5,Tram),(7,Bus)]
//Start = FindLinesR PPK // [(1,Bus),(4,Bus),(7,Bus)]
//Start = FindLinesR TTK // [(1,Bus),(3,Metro),(5,Tram),(6,Metro),(7,Bus)]



/*------------------------------------*/

/* 3.3. Write a function that takes two stops and returns the 
(transport,number) pairs for the lines that go between them.
If there is no such line, return an empty list. */
FindLinesB :: Stop Stop -> [(Transport,Int)]
FindLinesB st1 st2 = [ (x.transport,x.number) \\ x <- Linelist | (isMember st1 x.stops) && (isMember st2 x.stops)]
//Start = FindLinesB IK BGGYK // [(Bus,1),(Tram,2),(Metro,3),(Tram,5)]
//Start = FindLinesB IK PPK // [(Bus,1),(Bus,4),(Bus,7)]
//Start = FindLinesB GTK BGGYK  // [(Bus,1)]

/*------------------------------------*/

/* 3.4. Write a function that takes two stops and a transport 
and returns the numbers of the lines that goes between them 
with the given transport. */
FindLinesT :: Stop Stop Transport -> [Int]
FindLinesT st1 st2 tr = [ x.number \\ x <- Linelist | (isMember st1 x.stops) && (isMember st2 x.stops) && (x.transport == tr)]
//Start = FindLinesT IK BGGYK Bus // [1]
//Start = FindLinesT IK BGGYK Tram // [2,5]
//Start = FindLinesT PPK AJK Metro // []

/*------------------------------------*/

/* 4. Sets - 20 points - each function of 5 points
Set is a type synonym for a list of any type a elements,
it has values in increasing order and no duplicates.
Define the Set type and the following functions on sets:

- add: takes an element and a set and adds the element 
to the set (if it is not already in the set).
- remove: takes an element and a set and removes the element 
from the set (if it is in the set).
- union: takes two sets and returns the union of two sets 
(the set of elements that are in either of the sets).
- intersection: takes two sets and returns the intersection 
of two sets (the set of elements that are in both of the sets).
After each operation the set still has values increased 
and no duplicates. */

:: Set a :== [a]

add :: a (Set a) -> (Set a) | Eq, Ord a
add a [] = [a]
add a set 
| isMember a set = set
= sort (removeDup(set ++ [a]))


//Start = add 5 [] // [5]
//Start = add 1 [2,3,4,5] // [1,2,3,4,5]
//Start = add 4 [1,2,5] // [1,2,4,5]
//Start = add 4 [1,2,3,4,5] // [1,2,3,4,5]
remove :: a (Set a) -> (Set a) | Eq a & Ord a //Ord, Eq a
remove a [] = []
remove a set 
| isMember a set = [x \\ x <- set | x <> a] 
= set

//Start = remove 3 [] // []
//Start = remove 4 [1,2,3,4,5] // [1,2,3,5]
//Start = remove 3 [1,2,4,5] // [1,2,4,5]

Union :: (Set a) (Set a) -> (Set a) | Ord a & Eq a
Union seta setb = sort(removeDup([x \\ x <- seta ] ++ [ y \\ y <- setb]))

//Start = Union [1,3,9] [2,4,6,8] // [1,2,3,4,6,8,9]
//Start = Union [] [1..5] // [1,2,3,4,5]



//Start = intersection [1,3,9] [1..5] // [1,3]
//Start = intersection [1,3,9] [2,4,6,8] // []

/*------------------------------------*/

/* 5. Planets - 10 points (age computation 5 points, ageOn 5 points)
People are now able to live on 5 planets of the Solar System. 
In order to issue their passport, we must calculate their age 
on the specific planet they want to live on.
Given the Planet algebraic data type, calculate each person's 
age on the specified Planet (use integer division).

Time in the Solar System:
Mercury 88 days
Venus 225 days
Mars 687 days
Jupiter 11.8 years
Saturn 29.4 years

If your age is 18 on Earth (it has 365 days) you are:
(18*365/88) = 74 years old on Mercury
(18/11.8) = 1 year old on Jupiter */

:: Planet = Mercury | Venus | Mars | Jupiter | Saturn 
:: Person = {firstname :: String, planet :: Planet, age :: Int}
instance == Planet
where 
	(==) Mercury Mercury = True
	(==) Venus Venus = True
	(==) Mars Mars = True
	(==) Jupiter Jupiter = True
	(==) Saturn Saturn = True
	(==) _ _ = False
	
people1 = {{firstname="Bob", planet=Mercury, age=34}, {firstname="Harry", planet=Venus, age=30},{firstname="Emilia", planet=Saturn, age=9}, {firstname="Leo", planet=Mars, age=89}}
people2 = {{firstname="U", planet=Saturn, age=20}, {firstname="A", planet=Venus, age=20}, {firstname="B", planet=Mercury, age=20}, {firstname="X", planet=Jupiter, age=20}, {firstname="Y", planet=Mars, age=20}}

ageAux :: Planet -> Int
ageAux p
| p == Mercury = 88
| p == Venus = 225
| p == Mars = 687
| p == Jupiter = 4307 
| p == Saturn = 9088 
= 0
age :: Person -> Int // takes a person and calculates his age on the planet that he wants to go 
age p = (p.age * 365) / (ageAux p.planet)
ageOn :: {Person} -> {Int}
ageOn arr = { age p \\ p <-: arr}

	
//Start = ageOn people1 // {141, 48, 0, 47}
//Start = ageOn people2 //{0, 32, 82, 1, 10}

/*------------------------------------*/

/* 6. Good tree - 10 points (isGood condition 5 points, counting 5 points)
Given a tree, return the number of good nodes. A Node is good if the value 
of the node is equal to the average of the values in its subtree including itself. 
The average should be calculated using integer division. 

E.g.			node 4: subtree average (4+8+5+0+1+6)/6 = 4 -> good
				node 5: subtree average (5+6)/2 = 5 -> good
	4			node 8: subtree average (8+0+1)/3 = 3 -> not good
   /  \			node 0: subtree average (0)/1 = 0 -> good
  8    5		node 1: subtree average (1)/1 = 1 -> good
 / \    \		node 6: subtree average (6)/1 = 6 -> good
 0  1    6		Result: 5   */

:: TreeT a = NodeT a (TreeT a) (TreeT a) | LeafT
tree1 = NodeT 2 (NodeT 10 (NodeT 3 (NodeT 24 LeafT LeafT) LeafT) (NodeT 6 LeafT (NodeT 7 LeafT LeafT))) (NodeT 8 (NodeT 15 LeafT (NodeT 10 LeafT (NodeT 14 LeafT LeafT))) (NodeT 13 LeafT (NodeT 16 LeafT LeafT)))
tree2 = NodeT 4 (NodeT 8 (NodeT 0 LeafT LeafT) (NodeT 1 LeafT LeafT)) (NodeT 5 LeafT (NodeT 6 LeafT LeafT))
getNode :: (TreeT Int) -> Int
getNode LeafT = 0
getNode (NodeT x l r) = 1 + getNode l + getNode r
ff :: (TreeT Int) -> [Int]
ff LeafT = [0]
ff (NodeT x l r) = [x] ++ ff l ++ ff r

gg :: (TreeT Int) -> Int
gg LeafT = 0
gg (NodeT x l r)
| (sum(ff (NodeT x l r)) / (getNode (NodeT x l r))) == x = 1 + gg l + gg r
= gg l + gg r

//Start = gg tree2

//Start = ff tree2
//isGood :: 
ex :: (TreeT Int) -> Int
ex LeafT = 0
ex (NodeT x _ _) = x

getSum :: (TreeT Int) -> Int
getSum LeafT = 0
getSum (NodeT x l r) = x +  getSum l + getSum r // ex l + ex r 

//Start = getSum tree2



goodNodes :: (TreeT Int) -> Int
goodNodes LeafT =  0
goodNodes (NodeT x l r) 
| x == (((getSum l) + (getSum r) + x ) /(1 + getNode l + getNode r) ) = 1 + goodNodes l + goodNodes r
= goodNodes l + goodNodes r

//Start = getNode tree2
//Start = goodNodes tree1 // 6
//Start = goodNodes tree2 // 5
//Start = goodNodes LeafT // 0

/*------------------------------------*/

/* 7. Ternary tree - 10 points (condition 5 points, counting 5 points)
Write a function that takes a ternary tree 
(a tree having 3 children for each node) and 
returns the number of nodes that have exactly 3 children.
E.g.:
                            1
                      /     |     \
                     2      3      4
                   / | \   / \    / | \
                  5  6 7  8  9  10 11 12
                 / \      |     |  
                13 14     15    16
                
the nodes 1,2,4 have exactly 3 children, so it returns 3 */

:: Tree a = Node a (Tree a) (Tree a) (Tree a) | Leaf

treeTrio1 :: Tree Int
treeTrio1 = Node 1 ( Node 2 (Node 5 (Node 13 Leaf Leaf Leaf) (Node 14 Leaf Leaf Leaf) Leaf) (Node 6 Leaf Leaf Leaf) (Node 7 Leaf Leaf Leaf)) (Node 3 (Node 8 (Node 15 Leaf Leaf Leaf) Leaf Leaf) (Node 9 Leaf Leaf Leaf) Leaf) (Node 4 (Node 10 Leaf Leaf Leaf) (Node 11 Leaf Leaf Leaf) (Node 12 (Node 16 Leaf Leaf Leaf) Leaf Leaf))

treeTrio2 :: Tree Int
treeTrio2 = Node 1 Leaf Leaf Leaf

isTernary :: (Tree Int) -> Bool
isTernary Leaf = False
isTernary (Node a Leaf y z) = False
isTernary (Node a x Leaf z) = False
isTernary (Node a x y Leaf) = False
isTernary (Node a x y z) = True

count :: (Tree Int) -> Int
count Leaf = 0
count (Node x a b c) 
| isTernary (Node x a b c) = 1 + count a + count b + count c 
= count a + count b + count c 

//Start = isTernary treeTrio2


//Start = count treeTrio1 // 3
//Start = count treeTrio2 // 0

/*------------------------------------*/

/* 8. Right child - 10 points
Write a function that takes a binary tree and returns 
the biggest node that has only right child.
E.g.: 
                            1
                      /           \
                     2             3
                   /   \         /   \
                  4     5       6     7
                   \     \     /     / \
                    9    11   12     14 15   

the biggest node that has only right child is 5. */

:: Tree2 a = Node2 a (Tree2 a) (Tree2 a) | Leaf2
check :: Tree2 Int
check = Node2 1 (Node2 3 Leaf2 Leaf2) (Node2 4 Leaf2 Leaf2)

treefun :: Tree2 Int
treefun = Node2 1 (Node2 2 (Node2 4 Leaf2 (Node2 9 Leaf2 Leaf2)) (Node2 5 Leaf2 (Node2 11 Leaf2 Leaf2))) (Node2 3 (Node2 6 (Node2 12 Leaf2 Leaf2) Leaf2) (Node2 7 (Node2 14 Leaf2 Leaf2) (Node2 15 Leaf2 Leaf2)))

oneRight :: (Tree2 Int) -> Bool 
oneRight (Node2 x l Leaf2) = False
oneRight (Node2 x Leaf2 Leaf2) = False
oneRight (Node2 x Leaf2 r) = True
oneRight (Node2 x l r) = False
oneRight Leaf2 = False 




kk :: (Tree2 Int) -> [Int]
kk Leaf2 = []
kk (Node2 x l r) 
| oneRight (Node2 x l r) = ([x] ++ kk l ++ kk r )
= (kk l ++ kk r )


//Start = oneRight check








biggestNode :: (Tree2 Int) -> Int
biggestNode t = maxList(kk t)

//Start = biggestNode treefun // 5

/*------------------------------------*/

/* 9. Comparisons - 20 points (10 points each operator)
Write a class Comparison that has the following binary operators >>>> and <<<<.

>>>> takes two arrays and returns true if the first array is greater than the second array
it compares them by first comparing the lengths of the arrays 
and then comparing the elements of the arrays.
if the lengths are not equal, it returns the result of the length comparison.
if the lengths are equal, it compares if each element of the first array 
is greater than the corresponding element of the second array.
if all the elements are greater, the sums of the first half of the elements 
of the first array and the second half of the elements of the second array are compared.

<<<< takes two arrays and returns true if the first array is less than the second array
it compares them by first comparing the lengths of the arrays and then comparing 
the elements of the arrays.
if the lengths are not equal, it returns the result of the length comparison.
if the lengths are equal, it compares if each element of the first array 
is less than the corresponding element of the second array.
if all the elements are less, the sums of the first half of the elements of the 
first array and the second half of the elements of the second array are compared.


eg1: [4,5,8,1] >>>> [2,3,4,0] // True
     length is same, so elements are compared
     4>2, 5>3, 8>4, 1>0 all are greater so sum is compared
     4+5=9 > 4+0=4  so True

eg2: [4,5,8,1] >>>> [2,3,4,0,1] // False
     length is not same, so length is compared
     4 is smaller then 5, so False

eg3: [4,5,9,7] >>>> [3,3,8,6] // False
     length is same, so elements are compared
     4>3, 5>3, 9>8, 7>6  all are greater so sum is compared
     4+5=9 < 8+6=14 so False

eg4: [3,3,8,6] <<<< [4,5,9,7] // True
     length is same , so elements are compared
     3<4, 3<5, 8<9, 6<7 , all are less so sum is compared
     3+3=6 < 9+7=18 so True */

class Comparisons a
where 
	(>>>>) :: [a] [a] -> Bool
	(<<<<) :: [a] [a] -> Bool
instance Comparisons Int
where 
	(>>>>) l1 l2 
	| length(l1) == length(l2) = and[ x > y \\ x <- l1 & y <- l2]
	= False
	(<<<<) l1 l2 
	| length(l1) == length(l2) = and[ x < y \\ x <- l1 & y <- l2]
	| length(l1) < length(l2) = True
	= False
	










//Start = [4,5,8,1] >>>> [2,3,4,0] // True
//Start = [4,5,8,1] >>>> [2,3,4,0,1] // False
//Start = [4,5,9,7] >>>> [3,3,8,6] // True
//Start = [4,5,8,1] <<<< [2,3,4,0] // False
//Start = [4,5,8,1] <<<< [2,3,4,0,1] // True
//Start = [4,5,9,7] <<<< [3,3,8,6] // False
//Start = [3,3,8,6] <<<< [4,5,9,7] // True

/*------------------------------------*/

/* 10. Maybe type - 10 points
Given a pair of integers in a tuple and two conditions.
If the first condition holds for the first tuple component and the second 
condition for the second tuple component, return in Just construct 
the pair of the sum and the product of the original tuple values.
If only first condition holds for the first tuple component, 
return in Just construct the pair of sum of the tuple elements and 0.
If only the second condition holds for the second tuple component,
return in Just construct the pair of product of the tuple elements and 1.
If none of the conditions are true, return Nothing. */

cc :: (Int,Int) -> Int
cc tup = fst tup + snd tup
dd :: (Int,Int) -> Int
dd tup = fst tup * snd tup

:: Maybe a = Just a | Nothing

checkMaybe :: (Int, Int) (Int -> Bool) (Int -> Bool) -> (Maybe (Int, Int)) 
checkMaybe tup cond1 cond2 
| (cond1 (fst tup)) && (cond2 (snd tup)) = Just(cc tup, dd tup)
= Nothing

//Start = checkMaybe (1,2) isEven isOdd // Nothing
//Start = checkMaybe (2,3) isEven isOdd // (Just (5,6))
//Start = checkMaybe (2,4) isEven isOdd // (Just (6,0))
//Start = checkMaybe (3,5) isEven isOdd // (Just (15,1))
//Start = checkMaybe (1,0) isEven isOdd // Nothing

/*------------------------------------*/