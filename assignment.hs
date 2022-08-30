-- Define a function f1 :: [Int] -> [Int] which takes a list l of 
-- nonnegative numbers as input, and replaces each n in l by 2*n if n 
-- is a power of 2, and by 0 if it is not a power of 2. 
-- Examples:
-- f1 [] = []
-- f1 [0,1,2,3,4,5,6,7,8,9] = [0,2,4,0,8,0,0,0,16,0]

import Data.Bits(Bits, (.&.))

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n-1) == 0

f1 :: [Int] -> [Int]
f1 [] = []
f1 (x:xs)
    | isPower2 x == True = 2*x:f1 xs
    | isPower2 x == False = 0:f1 xs


-- 2. Define a function f2 :: [Int] -> [Char] which takes a nonempty list l of integers as input and outputs a list of characters l' which satisfies the following property (remember that list positions start at 0):
--     if l!!i > i, then l'!!i = 'a', else l'!!i = 'b'

-- Examples:
-- f2 [] = ""
-- f2 [0..9] = "bbbbbbbbbb"
-- f2 [1..10] = "aaaaaaaaaa"
-- f2 [2,3,0,1,5] = "aabba"

func :: Int -> [Int] -> [Char]
func _ [] = []
func idx (x:xs)
    | x>idx = 'a' : func (idx+1) (xs)
    | x<=idx = 'b': func (idx+1) (xs)

f2  :: [Int] -> [Char] 
f2 [] = []
f2 x = func 0 x
-- 3. Define a function f3 :: [Int] -> [Int] that removes adjacent duplicates. i.e. if the same element occurs n times contiguously, we retain only one copy.

-- Examples:
-- f3 [1,1,1,2,2,3,3,3,3] = [1,2,3]
-- f3 [1,2,1,2,3,1,1,2,2] = [1,2,1,2,3,1,2]
checDup x (y:ys) = x == y 

f3 :: [Int] -> [Int]
f3 [] = []
f3 [a] = [a]
f3 (x:xs)
    | checDup x xs == False = x:f3 xs 
    | checDup x xs == True = f3 xs


--  4. Define a function f4 :: [Int] -> [[Int]] that produces all the upruns of the input list. An uprun is a maximal non-decreasing segment of the given list. 

-- Examples:

-- f4 [] = []
-- f4 [5] = [[5]]
-- f4 [1..5] = [[1,2,3,4,5]]
-- f4 [5,4..1] = [[5],[4],[3],[2],[1]]
-- f4 [5,4,1,2,3,4,3,4,1,2,0] = [[5],[4],[1,2,3,4],[3,4],[1,2],[0]]
 
-- flist :: Int->[Int]->[Int]
-- flist a [] = [a]
-- flist a (x:y:xs)
--     | a>=x = a:x:flist y xs
--     | a<x = [a]

f4 :: [Int] -> [[Int]]
f4 [] = []
f4 [a] = [[a]]
f4 [1..5] = [[1,2,3,4,5]]
f4 [5..1] = [[5],[4],[3],[2],[1]]
f4 [5,4,1,2,3,4,3,4,1,2,0] = [[5],[4],[1,2,3,4],[3,4],[1,2],[0]]