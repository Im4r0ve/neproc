-- 4. úloha
--
-- 1) Implementujte kódování a dekódování RLE (https://en.wikipedia.org/wiki/Run-length_encoding):

-- >>> rleEncode "hello"
-- [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
--

rleEncode :: (Eq a) => [a] -> [(Int, a)]
rleEncode [] = []
rleEncode (x:xs) = reverse(rleEncode' xs [(1,x)])

rleEncode' :: (Eq a) => [a] -> [(Int, a)] -> [(Int, a)]
rleEncode' [] a = a
rleEncode' (x:xs) a = rleEncode' xs (encode x a)

encode :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
encode x ((first,second): xs) =  
    if x == second
    then ((first + 1,second): xs)
    else (1, x) : ((first,second): xs)

-- >>> rleDecode [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
-- "hello"

rleDecode :: [(Int, a)] -> [a]
rleDecode x = foldl (decode) [] x
    where
        decode x (0,_) = x
        decode x (i,ch) = decode (x ++ [ch]) (i-1, ch)
-- 2) Definujte nekonečný seznam všech prvočísel. Pokuste se o efektivní řešení.

-- >>> take 5 primes
-- [2,3,5,7,11]
--
primes :: [Integer]
primes = 2 : 3 : filter (isPrime primes) [5,7..]

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ = False --not needed
isPrime (x:xs) n
    | n < x*x = True
    | (n `mod` x) == 0 = False
    | otherwise = isPrime xs n

-- 3) Implementujte mergesort.
-- Prvním argumentem je funkce, která provádí porovnávání.
--
-- >>> sortWith (<) [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (>) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith p [] x = x
mergeWith p x [] = x
mergeWith p (x:xs) (y:ys)
    | (p x y) = x : mergeWith p xs (y:ys)
    | otherwise = y : mergeWith p (x:xs) ys 

splitInHalf :: [a] -> ([a],[a])
splitInHalf xs = (take n xs , drop n xs)
    where 
        n = (length xs) `div` 2

sortWith  :: (a -> a -> Bool) -> [a] -> [a]
sortWith p xs
    | length xs > 1 = mergeWith p (sortWith p lhs) (sortWith p rhs)
    | otherwise = xs
    where 
        (lhs , rhs) = splitInHalf xs


-- BONUS)
--
-- Implementujte následující funkce:

-- combinations n x vygeneruje seznam všech kombinací délky n ze seznamu x.
-- Na pořadí kombinací ve výsledném seznamu nezáleží.
--
-- >>> combinations 2 "abcd"
-- ["ab","ac","ad","bc","bd","cd"]
--
combinations :: Int -> [a] -> [[a]]
combinations n xs
    | n < 1         = []
    | n > length xs = []
    | otherwise     = combine n xs
    where   
        combine 0 _      = [[]]
        combine _ []     = []         
        combine m (y:ys) = map (y:) (combine (m-1) ys) ++ combine m ys
-- permutations x vygeneruje seznam všech permutací. Na pořadí permutací ve
-- výsledném seznamu nezáleží.
--
-- >>> permutations "abc"
-- ["abc","bac","bca","acb","cab","cba"]
--
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- choose xs, zs <- permutations ys]
  where choose []     = []
        choose (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- choose xs ]


-- Pomocí těchto funkcí definujte "variace" (občas najdete v české literatuře,
-- v angličtině pro to termín asi neexistuje): kombinace, kde záleží na pořadí
--
-- >>> variations 2 "abc"
-- ["ab","ba","ac","ca","bc","cb"]
--
variations :: Int -> [a] -> [[a]]
variations _ [] = []
variations n xs = concat [permutations comb | comb <- combinations n xs]

