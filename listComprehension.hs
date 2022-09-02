-- Square all numbers in a list
sqr :: [Int] -> [Int]
sqr a = [x*x | x <- a]

-- Find the divisors of n
divisors :: Int -> [Int]
divisors n = [x | x<-[1..n],(mod n x)==0]

-- Check for prime numbers below n
primes :: Int -> [Int]
primes n = [x|x<-[1..n], (divisors x == [1,x])]

-- Making a list of tuples from two lists
lot ::[Int]-> [Int] -> [(Int,Int)]
lot x y = [(a,b)|a<-x, b<-y]

-- Generating pythagorean triplets below n 
-- (a,b,c) is a pythagorean triplet if a^2+b^2=c^2
pyth :: Int -> [(Int,Int,Int)]
pyth n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x+y*y==z*z, x<=y,y<=z]

-- Duplicating the concat function
-- It basically takes in a list of lists and merges
-- them into one list. 
checkconcat :: [[Int]] -> [Int]
checkconcat l = [x | y<-l, x<-y]

-- Given a list of lists, extract all even length non empty lists
extract :: [[Int]]->[[Int]]
extract l = [x | x<-l,(mod (length x) 2 == 0), (length x>0)]
