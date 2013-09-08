import Data.List

length' :: [a] -> Int
length' (x:xs) = 1 + length' xs
length' []     = 0

sum' :: Num a => [a] -> a
sum' (x:xs) = x + sum' xs
sum' []     = 0

average' :: Fractional a => [a] -> a
average' xs = sum'(xs) / (fromIntegral $ length'(xs))

palindrome :: String -> String
palindrome xs = xs ++ (reverse xs)

isPalindrome :: String -> Bool
isPalindrome []    = True
isPalindrome [x]   = True
isPalindrome [x,y] = x == y
isPalindrome xs    =
	let h = head xs
	    l = last xs
	    i = init xs
	    m = tail i
	in h == l && (isPalindrome m)

byLength :: [a] -> [a] -> Ordering
byLength xs1 xs2 = compare (length xs1) (length xs2)

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy byLength xs