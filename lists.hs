-- A few programs to check understanding on lists in Haskell
-- length of a list
len_list :: [Int] -> Int
len_list [] = 0
len_list (x:xs) = 1+len_list(xs)

-- Find if an element exists in a list
find_elem :: Int -> [Int] -> Bool
find_elem _ [] = False
find_elem e (x:xs)
  | (x == e) = True
  | otherwise = find_elem e xs

-- A function to remove all duplicates from a list
del_dup :: [Int] -> [Int]
del_dup [] = []
del_dup (x:xs)
    | (find_elem x xs == False) = x:del_dup xs
    | (find_elem x xs == True) = del_dup xs


-- A function to check if a list is in ascending order
ascend :: [Int] -> Bool
ascend [] = True
ascend [a] = True
ascend (x:y:xs)
    | x<=y = ascend(y:xs)
    | x>y = False

-- Attaching two lists
attachlists :: [Int] -> [Int] -> [Int]
attachlists [] l = l
attachlists l [] = l
attachlists (x:xs) l = x:(attachlists xs l)
-- or, more easily
attach :: [Int] -> [Int] -> [Int]
attach x y = x++y

-- Reverse a list
revlist :: [Int] -> [Int]
revlist [x] = [x]
revlist (x:xs) = revlist xs ++ [x]

-- Alternating List
-- Either the list will have alternate elements going up first then down or down first then up.
alternating :: [Int] -> Bool
alternating x  = updown x || downup x

updown :: [Int] -> Bool
updown [] = True
updown [x] = True
updown (x:y:xs) = (x>=y) && downup(y:xs)


downup :: [Int] ->Bool
downup [] = True
downup [x] = True
downup (x:y:xs) = (x<y) && updown(y:xs)

--Implementing the drop n l builtin function
  drop2 n l
  | n<=0 || null l = l
  | otherwise = drop2 (n-1) (tail(l))
saf l = tail(l)
