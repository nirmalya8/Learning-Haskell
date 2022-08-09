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


