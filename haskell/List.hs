-- [1] Find the last box of a list.

last' :: [a] -> a
last' [] = error "undefined"
last' [x] = x
last' (_:xs) = last' xs

--  Find the last but one box of a list.
last2 :: [a] -> [a]
last2 [] = []
last2 x@(_:xs) = if length(x) <= 2 then x else last2 xs

-- Find the K'th element of a list.
nth :: [a] -> Int -> a
nth l k = nth' l k 1
	where
		nth' (x:xs) n m
			| n == m = x
			| otherwise = nth' xs n (m + 1)


