-- Finding the factorial of a number
factorial :: Int -> Int
factorial a
          | a < 0 = 0
          | (a==0)  = 1
factorial n = n * factorial(n-1)

-- Finding the Greatest Common Divisor of a and b
gcd1 :: Int -> Int -> Int
gcd1 a 0 = a
gcd1 a b
    | a>=b = gcd1 b (mod a b)
    | a<b = gcd1 b a

-- Finding the largest divisor of n other than n itself
large :: Int -> Int
large n = lardiv n (n-1)

lardiv :: Int -> Int -> Int
lardiv a i
  | (mod a i) == 0 = i
  | otherwise = lardiv a (i-1)


