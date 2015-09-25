f::Int-> Int -> Int
f x y = if x < 0 then x
		else f (x-1) (f x y)

ones::[Int]
ones = 1:ones

from n = [n..]
from n = n:(from (n+1))
indirect_nats::[Int]
indirect_nats = from 0

add::[Int]-> [Int] -> [Int]
add (x:xs) (y:ys) = (x + y): (add xs ys)

mult (x:xs) (y:ys) = (x*y): (mult xs ys)

twos = 2:twos
nats = 0 : (add ones nats)
evens = 0: (add twos evens)
fibs = 0 : 1 : (add fibs (tail fibs))
pows = 1: (mult twos pows)

primes = sieve [2..]
sieve (x:xs) = x : [a| a <- sieve xs, mod a x /= 0]

fromStep::Int -> Int -> [Int]
fromStep n m = n:fromStep m (m + m - n)

pythagTriples :: [(Int,Int,Int)]
pythagTriples = [ (x,y,z) | z <- [2 .. ], y <- [2 .. z-1], x <- [2 .. y-1], x*x + y*y == z*z ]

primes2 = sieve [2 .. ]
sieve2 (x:xs) = x : sieve2 [ y | y <- xs, y `mod` x > 0]

fact::Integer -> Integer
fact 1 = 1
fact n = n * (fact (n-1))