-- 1. Write the converse of fromList for the List type: a function that takes a
--    List a and generates a [a].

import Data.List (sortBy, groupBy, intersperse)
import Data.Function (on)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList (Cons x xs) = x:toList(xs)
toList _ = []

-- 2. Define a tree type that has only one constructor, like our Java example.
--    Instead of the Empty constructor, use the Maybe type to refer to a node's
--    children.

data Tree a = Tree a (Tree (Maybe a)) (Tree (Maybe a))
              deriving (Show)

-- 1. Write a function that computes the number of elements in a list. To test
--    it, ensure that it gives the same answers as the standard length
--    function.

len (x:xs) = 1 + len xs
len [] = 0

-- 2. Add a type signature for your function to your source file. To test it,
--    load the source file into ghci again.

len :: [a] -> Int

-- 3. Write a function that computes the mean of a list, i.e. the sum of all
--    elements in the list divided by its length. (You may need to use the
--    fromIntegral function to convert the length of the list from an integer
--    into a floating point number.)

mean :: [Double] -> Double
mean l = snd(inner l) / fst(inner l)
         where inner [] = (0,0)
               inner (x:xs) = (fst(inner xs) + 1, snd(inner xs) + x)

-- 4. Turn a list into a palindrome, i.e. it should read the same both
--    backwards and forwards. For example, given the list [1,2,3], your
--    function should return [1,2,3,3,2,1].

palindrome :: [a] -> [a]
-- palindrome (xs) = xs ++ reverse xs
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]

-- 5. Write a function that determines whether its input list is a palindrome.

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
-- isPalindrome xs = take half xs == (reverse $ drop half xs)
--                   where half = div 2 $ length xs
-- isPalindrome xs = fst $ inner xs == reverse $ snd $ inner xs
--                   where inner xs = splitAt (div 2 $ length xs) xs
-- isPalindrome xs = fst inner xs == reverse . snd inner xs
--                   where inner xs = splitAt (div 2 $ length xs) xs

-- 6. Create a function that sorts a list of lists based on the length of each
--    sublist. (You may want to look at the sortBy function from the Data.List
--    module.)
sortLists = sortBy (compare `on` length)

-- 7. Define a function that joins a list of lists together using a separator
--    value.

joinLists = intersperse

-- 8. Using the binary tree type that we defined earlier in this chapter, write
--    a function that will determine the height of the tree. The height is the
--    largest number of hops from the root to an Empty. For example, the tree
--    Empty has height zero; Node "x" Empty Empty has height one; Node "x"
--    Empty (Node "y" Empty Empty) has height two; and so on. 26 comments

-- treeHeight :: Tree t -> Int
-- treeHeight Nothing = 0
-- treeHeight (Just (Tree _ l r)) = 1 + max (treeHeight l) (treeHeight r)

-- treeHeight :: Tree t -> Int
-- treeHeight Empty = 0
-- treeHeight (Node _ Empty Empty) = 1
-- treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- 9. Consider three two-dimensional points a, b, and c. If we look at the
--    angle formed by the line segment from a to b and the line segment from b
--    to c, it either turns left, turns right, or forms a straight line. Define
--    a Direction data type that lets you represent these possibilities. 18
--    comments

-- 10. Write a function that calculates the turn made by three 2D points and
--     returns a Direction. 41 comments

-- 11. Define a function that takes a list of 2D points and computes the
--     direction of each successive triple. Given a list of points [a,b,c,d,e],
--     it should begin by computing the turn made by [a,b,c], then the turn
--     made by [b,c,d], then [c,d,e]. Your function should return a list of
--     Direction. 15 comments

-- 12. Using the code from the preceding three exercises, implement Graham's
--     scan algorithm for the convex hull of a set of 2D points. You can find
--     good description of what a convex hull. is, and how the Graham scan
--     algorithm should work, on Wikipedia. 49 comments
