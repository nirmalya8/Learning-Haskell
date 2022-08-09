-- Finding the square of a number
sqr :: Int -> Int
sqr x = x*x

-- Finding the XOR(Exclusive OR) Approach 1(Using boolean logic)
xor :: Bool -> Bool -> Bool
xor a b = (a && (not b)) || ((not a) && b)

-- Approach 2(Using pattern matching)
xor2 :: Bool -> Bool -> Bool
xor2 (True) (False) = True
xor2 (False) (True) = True
xor2 a b = False

-- Check if three integers are in ascending order
inorder :: Int -> Int -> Int -> Bool
inorder a b c = (a<=b) && (b<=c)



