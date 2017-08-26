import Data.List
import Debug.Trace

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
-- roots.
quadraticRoots :: Floating t => t -> t -> t -> (t, t)						-- type signature
quadraticRoots a b c = ((-b + det) / (divs), (-b - det) / (divs))   				-- (first element, second element)
		       where det  = sqrt(b*b-4*a*c)		    				-- (discriminant value computation)
			     divs = 2*a				    				-- (divisor ration)	

-- end of function


-- Problem 2
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.
iterateFunction :: (a -> a) -> a -> [a]								-- type signature
iterateFunction f z = z : iterateFunction f (f z)						-- infinite lazy list generation 

-- end of function

-- Problem 3
-- Using iterateFunction return infinite list containing 
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.
multiples n = (map (\ elem -> elem*n) (iterateFunction (\ elem -> elem+1) 0))			-- infinite list for addition then map elem for mul 

-- end of function

-- Problem 4
-- Use iterateFunction to return an infinite list containing list 
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.
hailstones :: Integral a => a -> [a]								-- type signature
hailstones n = (iterateFunction (\ n -> 							-- store result
				if odd n							-- check for odd int higher order fun 
				then 3*n+1 							-- compute for odd 
				else n `div` 2) 						-- compute for even
				n)								-- given parameter

-- end of function

-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List
hailstonesLen :: Integral a => a -> Int								-- type signature
hailstonesLen n = case elemIndex 1 (hailstones n) of						-- case check
                        	 Just position  -> position + 1					-- add 1 for this match	
                        	 Nothing -> (-1)						-- negative for not match

-- end of function

-- Problem 6
-- Given a list of numbers, return sum of the absolute difference
-- between consecutive elements of the list.
-- May NOT use recursion.
sumAbsDiffs :: Num a => [a] -> a										-- type signature 
sumAbsDiffs numberList = foldl (+) 0 (map (\ elem -> abs elem) (zipWith (-) numberList (tail numberList)))	-- fold for add, map for abs, zipwith for consecutive nums 

-- end of function


-- Problem 7
-- The x-y coordinate of a point is represented using the pair (x, y).
-- Return the list containing the distance of each point in list
-- points from point pt.
-- May NOT use recursion.
distances :: Floating b => (b, b) -> [(b, b)] -> [b]					  	         -- type signature
distances pt points = [sqrt(((fst pt-x2)*(fst pt-x2)) + ((snd pt-y2)*(snd pt-y2))) | (x2,y2) <- points]  -- list comprehension amde it easy

-- end of function

-- Problem 8
-- Given a list of coordinate pairs representing points, return the
-- sum of the lengths of all line segments between successive 
-- adjacent points.
-- May NOT use recursion.
sumLengths :: Floating a => [(a, a)] -> a												        -- type signature
sumLengths pointsList = foldl (+) 0 (zipWith (\ (x1,y1) (x2,y2) -> sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))) pointsList (tail pointsList))	-- fold for add, zipwith for cons terms 

-- end of function

-- Problem 9
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c = [index | (index, elemSearch) <- zip [0..] s, elemSearch == c]				-- list comprehension with zip

-- end of function

-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 10
-- Fold tree to a single value.  If tree is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree; if it is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1											-- type signature
foldTree treeFn leafFn (Leaf tree) = leafFn tree												-- check for leaf and apply leaf function
foldTree treeFn leafFn (Tree leftTree elem rightTree) = treeFn (foldTree treeFn leafFn leftTree) elem (foldTree treeFn leafFn rightTree)	-- check for Tree and apply tree function

-- end of function
 
-- Problem 11
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
flattenTree :: Tree a -> [a]											-- type signature
flattenTree tree = foldTree (\ left elem right -> left ++ [elem] ++ right) (\ elem -> [elem]) tree		-- leaf and tree function apply 

-- end of function


-- Problem 12
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]										-- type signature			
catenateTreeLists tree = foldr (++) [] (flattenTree tree)							-- use foldr to append tree elements

-- end of function

