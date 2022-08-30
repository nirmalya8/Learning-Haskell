-- In this file we aim to write the functions map and filter
-- and solve some basic problems using these two functions. 
import Data.Char
-- Defining map as map2
-- Map is a function which takes a list and a function as 
-- inputs applies that function on each element of the list 
-- and gives a list as the output.
map2 :: (a->b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x: map2 f xs

-- Given a list of lists, find the sum of the lengths of each
-- sublist. Eg. [[1,2,3],[1,2],[1]] = 6
lengthSubList :: [[a]] -> Int
lengthSubList [] = 0
lengthSubList (x:xs) = length x + lengthSubList xs

-- Now writing the same function using map 
sublength :: [[a]] -> Int
sublength [] = 0
sublength a = sum(map length a) 

-- The filter function takes in a property P and a list.
-- It gives a list as output which contain only those 
-- elements satisfying the property P.

filter2 :: (a->Bool)->[a]->[a]
filter2 _ [] = []
filter2 p (x:xs) 
  | p x == True = x:filter2 p xs
  | otherwise = filter2 p xs

is_even :: Int -> Bool
is_even x = (mod x 2)==0

-- Extract all vowels from a String and capitalize them
capitalize :: Char -> Char
capitalize ch
  | (ch>='a' && ch<='z') = chr(ord(ch)-(ord('a')-ord('A')))
  | otherwise = ch

is_vow :: Char -> Bool
is_vow c = (c=='a' || c=='e' || c=='i' || c=='o' || c=='u')

capVow :: [Char] -> [Char]
capVow [] = []
capVow l = map capitalize (filter is_vow l)

-- Find the squares of even numbers in a list
sqr :: Int -> Int
sqr x = x*x
sq_even :: [Int] -> [Int]
sq_even l = map sqr (filter is_even l)
