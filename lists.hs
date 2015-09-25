encode:: String -> [(Char, Int)]
encode list = encodeHelp list 1
	where
	encodeHelp::String -> Int -> [(Char, Int)]
	encodeHelp [] _ = []
	encodeHelp (x:xs) n 
		|length (x:xs) == 1 = [(x, 1)]
		|x == (head xs) = encodeHelp xs (n+1)
		|x /= (head xs) = (x, n) : encodeHelp xs 1
		
decode::[(Char, Int)] -> String
decode [] = []
decode (x:xs) = repeatCh (fst x) (snd x) ++ decode xs
	where
	repeatCh:: Char -> Int -> String
	repeatCh ch n = [ch | i <- [1..n]]
	
reverseList:: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = helper [x] (reverse xs)
	where
	helper:: [a] -> [a] -> [a]
	helper a [] = a
	helper a (x:xs) = (x:xs) ++ a
	
rotate::[a] -> Int -> [a]
rotate [] _ = []
rotate list 0 = list
rotate list n = (last list) : rotate (init list) (n-1)

stringCount::String -> [(Char, Int)]
stringCount [] = []
stringCount (x:xs) = (x, charCount (x:xs) x) : (stringCount[ch| ch <- xs, ch /= x])
	where
	charCount:: String -> Char -> Int
	charCount list chr = length [chr| a <- list, a == chr]
 
permCount:: [a] -> Int
permCount [] = 0
permCount list = fact (length list)
	where
	fact 1 = 1
	fact n = n* fact(n-1)
	
varCount:: [a] -> Int -> Int
varCount list k = div (fact (length list)) (fact (length list - k))
	where
	fact 1 = 1
	fact n = n* fact(n-1)

findDividers:: [Int] -> [Int]
findDividers [] = []
findDividers list = clear [a | i<- list, a <- [1..i], mod i a == 0]
	where
		clear:: [Int] -> [Int]
		clear [] = []
		clear (x:xs) = x : clear[a | a <- xs, a /= x]

smallToBig:: String -> String
smallToBig str = [if ch>= 'a' && ch <= 'z' then toEnum(fromEnum ch + fromEnum 'A' - fromEnum 'a') else ch | ch <- str]

triangles :: Integer -> [(Integer,Integer, Integer)]
triangles n = [ (a,b,c) | a<-[1..n], b <- [1..n], c <- [1..n], a+b >c, a+c >b, b+c > a]


scalarMult:: [Int] -> [Int] -> Int
scalarMult lst1 lst2 = sum [a * b | (a,b) <- zip lst1 lst2]

permutations:: [a] -> [[a]]
permutations [] = []
permutations (x:xs) = (x:xs) : [ a: (y:ys) | (a,(y:ys)) <- selections xs, a <- (x:xs) ]


selections:: [a] -> [(a, [a])]
selections []   = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

matrix:: [[Int]] -> [[Int]] -> [[Int]] 
matrix matr1 matr2 = [[scalarMult vect1 vect2 | vect2 <- transpose matr2] | vect1 <- matr1]

transpose::[[a]] -> [[a]]
transpose (x:xs) 
	| length x == 1 = [[head list | list <- (x:xs)]]
	|otherwise =      [head list | list <- (x:xs)] : transpose [ ys | (y:ys) <- (x:xs)]

permutations2 []  = [[]]
permutations2 (x:xs) = [(y:ys) | (y,xs) <- picks (x:xs), ys <- permutations xs]
  where
    picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]

{-

permutations:: [Int] -> [[Int]]
permutations [] = [[]]
permutations (x:xs) = [[i : clear i ys] | i <- (x:xs), (y:ys) <- (x:xs)]
	where
		clear:: Int -> [Int] -> [Int]
		clear i list = [b | b <- list, b /= i]
		

-}