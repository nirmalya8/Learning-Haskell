import Data.List
-- Define a function subSeq :: String -> String -> Bool which checks
-- whether the first argument is a subsequence of the second. 
-- A subsequence is obtained by deleting some letters in a string
-- and retaining the other characters in the same order as in the
-- original string.

-- Test cases:
-- subSeq "ab" "abc" = True
-- subSeq "ab" "acb" = True
-- subSeq "ab" "bca" = False
-- subSeq ""   "bea" = True
-- subSeq "ba" "ba"  = True

findchar _ "" = False
findchar c (x:xs)
  | x==c = True
  | otherwise = findchar c xs
subSeq :: String -> String -> Bool
subSeq "" _ = True
subSeq (x:xs) a
  | (findchar x a == True) = subSeq xs a
  | (findchar x a == False) = False

-- 2. Define a function subWord :: String -> String -> Bool which 
-- checks whether the first argument is a subword of the second. 
-- A subword is obtained by deleting some number (possibly 0) of 
-- letters at the left end and right end in a string and retaining 
-- the other characters in the same order.

-- Test cases:
-- subWord "ab" "abc" = True
-- subWord "ab" "acb" = False
-- subWord "ca" "bca" = True
-- subWord ""   "bea" = True
-- subWord "ba" "ba"  = True

subWord :: String -> String -> Bool
subWord a b  = isInfixOf a b

get_length :: [[Int]]->[Int]
get_length x = map length x

--(a) Define a function isMatrix :: [[a]] -> Bool that checks if 
--a list of lists is a valid matrix (nonzero number of rows, each
--of the same nonzero length).

--Test cases:
--isMatrix [] = False
--isMatrix [[],[],[]] = False
--isMatrix [[2,3], [4,5], [6,7]] = True 
--isMatrix [[2,3,4,5,6,7]] = True
isMatrix :: [[ a ]] -> Bool
isMatrix [] = False
isMatrix (r: rs ) = check ( length r) rs
  where
    check n [] = True
    check n (r: rs ) = ( length r == n) && check n rs
