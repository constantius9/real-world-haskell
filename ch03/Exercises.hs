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
isPalindrome xs =
	all (uncurry (==)) $ zip xs (reverse xs)

byLength :: [a] -> [a] -> Ordering
byLength xs1 xs2 = compare (length xs1) (length xs2)

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy byLength xs