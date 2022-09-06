-- The marks of all students in different assignments is given
-- as a list of lists. Find the total marks of each student. 
-- [[1,2],[3,6]]=[4,8]
marks :: [[Int]] -> [Int]
marks [x] = x
marks (x:xs) = zipWith (+) x (marks xs)


