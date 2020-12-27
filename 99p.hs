-- 1
myLast :: [x] -> x
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myLength :: [x] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 3 
elementat :: [a] -> Int -> a 
elementat [] _ = error "Empty list"
elementat (x:_) 1 = x
elementat (_:xs) y = elementat xs (y-1)

elementat' :: [a] -> Int -> a
elementat' x i
  | length x < i = error "Index out of bounds"
  | otherwise = x !! (i-1)

-- 4
myLength :: [a] -> Int
myLength a = foldl (\acc x -> acc + 1) 0 a

myLength' :: [a] -> Int
myLength' = sum . map (\_ -> 1) 

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' xs = foldl (\acc x -> x:acc) [] xs

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (myReverse xs) == xs

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- 8
-- (what is this?)
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

data Tree a = Empty
			| Node (Tree a) a (Tree a)
   deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

-- 8
compress :: (Eq a) => [a] -> [a]
compress = foldl (\acc x -> acc ++ (compress' [x] acc)) []

compress' :: (Eq a) => [a] -> [a] -> [a] -- this seems heretical since length [a] must == 1...w/e
compress' x [] = x
compress' x acc = if (head x) == (last acc) then [] else x

-- 8 (cleaner?)
compress'' :: (Eq a) => [a] -> [a]
compress'' [] = []
compress'' xs =
	foldl (\acc x -> c'' acc x) [head xs] xs
	where
		c'' acc x = if last acc == x then acc else acc ++ [x]

-- 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = tup : pack rest
	where
		(tup, rest) = span (==x) (x:xs)

-- 10
-- this is a good time to learn to use Maybe etc.
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = [(0, undefined)]
encode xs = map (\x -> tup x) $ pack xs
	where
		tup x = (length x, head x)

-- 11
data Dupe a = Multiple Int a | Single a 
	deriving (Eq, Show)

encodeModified :: (Eq a) => [a] -> [Dupe a]
encodeModified [] = []
encodeModified xs = let 
				d [x] = Single x
				d xs@(x:_) = Multiple (length xs) x -- this also matches x:[], so must be 2nd pattern match
	in map (\x -> d x) $ pack xs

	{-
	   you could pattern match on
	   xs@(x:x':x'') = Multiple (length xs) x 
	   too
	-}

-- 12
decodeModified :: (Eq a) => [Dupe a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) = (replicate n x) ++ decodeModified xs

-- 13
encodeDirect :: (Eq a) => [a] -> [Dupe a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect xs@(x:xs') = encode' dupes : encodeDirect rest
	where
		(dupes, rest) = span (==x) xs
		encode' ls = if length ls > 1 then Multiple (length ls) x else Single x

-- 14
dupli :: (Eq a) => [a] -> [a]
dupli [] = []
dupli (x:xs) = replicate 2 x ++ dupli xs

-- 15
repli :: (Eq a) => [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli [x] 1 = [x]
repli (x:xs) n = x : repli [x] (n-1) ++ repli xs n

	-- too much work, try this:
repli' :: (Eq a) => [a] -> Int -> [a]
repli' xs n = concatMap (replicate n) xs -- map (replicate n) over xs and concat results

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = de xs n
	where 
		de [] _ = []
		de (y:ys) 1 = de ys n
		de (y:ys) i = y : de ys (i-1)

-- 17
split :: [a] -> Int -> ([a], [a])
split [] _ = (undefined, undefined)
split xs n = s' [] xs n
	where
		s' _ [] _ = (undefined, undefined) -- seems like a non-exhaustive pattern...
		s' xs ys 0 = (xs, ys)
		s' xs (y:ys) n = s' (xs ++ [y]) ys (n-1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r = drop (l-1) $ take r xs

-- 19
rotate :: [a] -> Int -> [a]
rotate xs i = drop i' xs ++ take i' xs
	where i' = i `mod` length xs -- e.g. on a 10-element list, left-rotate by 3 = right-rotate by 7

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = (x, xs')
	where 
		xs' = take (i-1) xs ++ drop i xs
		x = last $ take i xs

	-- recursively:

removeAt' :: Int -> [a] -> (a, [a])
removeAt' 1 (x:xs) = (x, xs)
removeAt' n (x:xs) = (l, x:r)
	where (l, r) = removeAt (n-1) xs

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt a xs 1 = a : xs
insertAt a (x:xs) n = x : insertAt a xs (n-1)

-- 22
range :: Int -> Int -> [Int]
range x y
	| x < y = x : range (x+1) y
	| x > y = x : range (x-1) y
	| x == y = [x]

-- 23
--
-- 24
--
-- 25
--
-- 26
data Tree a = Empty
			| Node (Tree a) a (Tree a)
			deriving (Eq, Ord, Show)

toTree :: [a] -> Tree a
toTree [] = Empty
toTree [x] = Node Empty x Empty
toTree (x:x':xs) = Node (toTree [x']) x (toTree xs)

com' :: [a] -> [[a]]
com' [] = [[]]
com' xs@(x:xs') = com'' ++ (map (x:) com'')
	where com'' = com' xs'

com :: Int -> [a] -> [[a]]
com i = filter (\x -> length x == i) . com'

-- 27
import Data.Set

isSame :: [a] -> [a] -> Bool
isSame xs ys = fromList xs == fromList ys

group3 :: [a] -> [a]
group3 [] = [[]]
group3 xs = com 2 xs ++ com 3 xs ++ com 4 xs

-- 31
isPrime :: Int -> Bool
isPrime i = all (\x -> i `mod` x /= 0) [2..i']
	where i' = ceiling $ sqrt $ fromIntegral i
