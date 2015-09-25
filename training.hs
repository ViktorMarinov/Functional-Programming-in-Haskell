task1:: (Int -> Int) -> [Int] -> [Int]
task1 f list = [x | x <- list, elem (f x) list]

task2:: [String] -> Int
task2 [] = 0
task2 list = length [word | word <- list, hasVowel word]
	where
		hasVowel::String -> Bool
		hasVowel [] = False
		hasVowel (x:xs) = (elem x volews) || hasVowel xs
		volews = "aouei"

call2 = task2 ["asd","bbb","yt","bbb"]

task3::(Int -> Int) ->(Int ->Int) -> Int -> Int -> Bool
task3 f g a b = foldr (&&) True [((f.g) x == x) && ((g.f) x == x )| x <- [a..b]]

call4 = foldr (&&) True [True, True, True]
call3 = task3 (\x -> 2 + x) (\x -> x) 1 10
--call4 = (\x -> x + 2).(\x -> x - 2) 3

isInverse :: (Int->Int) -> (Int->Int) -> (Int, Int) -> Bool
isInverse f g (a, b) = 
 (length $ filter (\x -> (f.g) x == x && (g.f) x == x) interval) == size
  where
   interval = [a .. b]
   size = length interval


isAscending:: Int -> Bool
isAscending numb = helper numb 9
	where
		helper::Int->Int->Bool
		helper number lastDigit
			|number == 0 = True
			|mod (mod number 10) 2 == 0 = ((mod number 10) <= lastDigit) && helper  (div number 10) (mod number 10)
			|otherwise = helper (div number 10) lastDigit

generate::(Double -> Double) -> [Double] -> (Double->Double->Double)
generate f coef = (\x y -> (f (x + y)) * (foldl (+) 0 newCoef))
	where
		newCoef = [if(mod i 2 == 0) then coef!!i else -(coef!!i)| i <- [0..(length coef - 1)]]

