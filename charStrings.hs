-- A function capitalize to capitalize each letter
import Data.Char
capitalize :: Char -> Char
capitalize ch
  | (ch>='a' && ch<='z') = chr(ord(ch)-(ord('a')-ord('A')))
  | otherwise = ch

-- Capitalize a string
capStr :: String -> String
capStr "" = ""
capStr (x:xs) = capitalize x : capStr xs

