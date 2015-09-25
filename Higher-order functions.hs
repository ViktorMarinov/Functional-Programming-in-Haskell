import Data.Char

stream:: (Double->Double) -> [(Double->Double)]
stream f = [ helper x | x <- [0,1..]]
	where 
		helper::Int -> (Double -> Double)
		helper n = (\y -> if (mod n 2 == 0) then ( f (y + (fromIntegral n)/2)) else (f (((fromIntegral n)^3)*y)))

listStream::[Double]
listStream = [f 0 |f <- (take 5 (stream (\x -> x*x)))]

expandedAlphabet:: [Char]
expandedAlphabet = generateAlph ++ (reverse expandedAlphabet)
	where
		generateAlph::[Char]
		generateAlph = foldr (++) [] [makeString i | i <- ['a'..'z'] ]
		makeString::Char -> [Char]
		makeString ch = [ch | i <- [1..numb]]
			where
				numb = ord ch - ord 'a' + 1

maxFunction::[(Double -> Double)] -> (Double -> Double)
maxFunction list = foldr maxf (\x -> 0.0) list
	where
	 maxf::(Double->Double) -> (Double->Double) -> (Double->Double)
	 maxf f g = if ( f 0 > g 0) then f else g

maxDoubleFunction::[(Double->Double)] -> (Double->Double->Double)
maxDoubleFunction list = (\x y -> maximum [(f x)*(f y) | f <- list])
	
type Author = String
type Sales = Integer
type Rating = Double --число между 0 и 1
type Book = (Author, Sales, Rating)

popularity::Book -> Double
popularity (author, sales, rate) = ((fromIntegral sales) * rate)

mostPopular::[Book] -> (Book-> Double) -> Book
mostPopular list pop = foldr maxpop ("",0,0) list
	where
		maxpop bk1 bk2 = if (pop bk1) > (pop bk2) then bk1 else bk2


books = [("John", 5000, 0.9), ("Dragan", 10000, 0.6),("Pesho", 7000, 0.9)]

call = maxDoubleFunction [(\x->x * 4.0), (\x -> x^2), (\x -> x + 10)] 1 2
