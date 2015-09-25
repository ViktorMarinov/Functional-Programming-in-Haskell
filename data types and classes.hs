
data Car = Car
	{
		model::String,
		speed::Int
	}

instance Eq Car where
	(Car model1 speed1) == (Car model2 speed2) = model1 == model2

instance Ord Car where
	compare (Car model1 speed1) (Car model2 speed2)
		|speed1 > speed2 = GT
		|speed1 == speed2 = EQ
		|otherwise = LT

instance Show Car where
	show (Car model speed) = "Model: " ++ model ++ " Speed: " ++ show (speed)

instance Bounded Car where
	maxBound = Car "" 500
	minBound = Car "" 0

bentley = Car "Bentley" 330
supra = Car "Supra" 300

data Season = Spring | Summer | Autumn | Winter
	deriving (Eq,Ord,Enum,Show,Read)

instance Ord Shape where
	compare (Rectangle a1 b1) (Rectangle a2 b2)
		| a1 * b1 > a2 * b2 = GT
		| a1 * b1 == a2 * b2 = EQ
		|otherwise = LT
		
--data Packet = Test name | Packet 

data Point = Point Double Double
	deriving (Eq)

instance Show Point where
	show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"
	
data Shape = 
	Circle Point Double |
	Rectangle Point Double Double
	deriving (Eq)
	
instance Show Shape where
	show (Circle point rad) = "(" ++ show point ++ "," ++ show rad ++ ")"
	show (Rectangle point width heigth) = "(" ++ show point ++ "," ++ show width ++ "," ++ show heigth++ ")"


readPoint [] = [((Point 0 0), "")]
readPoint string = [((Point (findFirst string) (findSecond string)), string)]
	where
	findFirst:: String -> Double
	findFirst (x:xs)
		| x == '(' = read (head xs :" ")::Double
		| otherwise = findFirst xs
	findSecond:: String -> Double
	findSecond (x:xs)
		| x == ',' = read (head xs :" ")::Double
		| otherwise = findSecond xs
			

instance Read Point where
	readsPrec _ s = readPoint s
			
data Box a= 
	Empty |
	Holder a
	deriving(Show,Eq)
	
unpack::[Box a] -> [a]
unpack [] = []
unpack (Empty: xs) = unpack xs
unpack((Holder value): xs) = value : (unpack xs)

class YesNo a where
	yesNo:: a -> Bool

instance YesNo Int where
	yesNo x = (x >0)
	
instance YesNo Char where
	yesNo ch = ch /= '\0'
	
instance (YesNo a) => YesNo [a] where	
	yesNo str = (length str /= 0)

data Ticket = Ticket{
	city::String,
	price::Double
	}
