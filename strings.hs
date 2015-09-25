f:: Int -> Char -> String
f n ch
	|n== 0 = []
	|otherwise = (ch:f (n-1) ch)

isPrefix:: String -> String -> Bool
isPrefix small big 
	|length small > length big = False
	|small == [] = True
	|head small /= head big = False
	|otherwise = isPrefix (tail small) (tail big)

countSubStr:: String -> String -> Int
countSubStr small big 
	|small == [] = length big
	|length small > length big = 0
	|isPrefix small big = 1 + countSubStr small (tail big)
	|otherwise = countSubStr small (tail big)

countlett:: String -> Int
countlett str
	| head str > 'a' && head str < 'z' = 1 + countlett (tail str)
	|otherwise = countlett (tail str)

countCAPS:: String -> Int
countCAPS str
	|head str > 'A' && head str < 'Z' = 1 + countCAPS (tail str)
	|otherwise = countCAPS (tail str)

countLetters:: String -> (Int, Int)
countLetters str
	| str == [] = (0,0)
	|otherwise = (countlett str, countCAPS str)

removeFromList:: Integer -> [Integer] -> [Integer]
removeFromList element list
	|head list == element = tail list
	|otherwise = [head list] ++ (removeFromList element (tail list))

countIdent::[Integer] -> Integer
countIdent list 
	| length list == 1 = 0
	| head list == list!!1 = 1 + countIdent (tail list)
	| otherwise = countIdent (tail list)

reverseList::[Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = helper x (reverseList xs)
	where
		helper:: Int -> [Int] -> [Int]
		helper x [] = [x]
		helper x list2 = list2 ++ [x]

sumOfList::[Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + (sumOfList xs)

sumOfLists::[[Int]] -> [Int]
sumOfLists [] = []
sumOfLists listOflists = [(sum (head listOflists))] ++ sumOfLists (tail listOflists)

fList:: String -> String
fList [] = []
fList (x:xs) = [x] ++ fList[ch | ch <-xs, ch /= x]

newReverseList::[Int] -> [Int]
newReverseList [] = []
newReverseList (x:xs) =(reverseList xs) ++ [x]

