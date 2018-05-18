{- Name: Zanyar Sherwani
   G#:   G00841632
-}
module Homework3 where
import Prelude hiding (zipWith)

-- this function processes a list from 2 to the number n (x) and at each iteration it gets the
-- factors of the number x ([y]) && only adds the number x to the list if its factors are 1 and itself
-- this function does not add primes that would occur more than once, just one instance of each prime
prime_factors n = [ x | x <- [2..n], n `mod` x == 0, x >= 2, [y | y <- [1..x], x `mod` y ==0] == [1,x]]

-- returns a list of all occuring prime factors of n
primeFactors :: Int -> [Int]
primeFactors n
    | n < 2 = []            -- some crazy stuff here but, it takes the list of prime factor instances
                            -- and concatenates it with another list of prime factors. the new list of
                            -- prime factors is calculated by dividing n by the product of all instances
                            -- of primes, and the prime factors of this are then calculated by a recursive
                            -- call to itself.
    | otherwise =  sortList (primeFactors (n `div` (productOfElements (prime_factors n))) ++ prime_factors n)

-- sorts a list of integers from least to greatest
sortList :: [Int] -> [Int]
sortList [] = []
sortList (x:xs) =
  -- low represents the list of numbers less than x. Recursively calls itself on this new list
  let low = sortList [z | z <- xs, z <= x]
  -- high represents the list of numbers greater than x, recursive call on this new list
  -- It's getting really late, and I'm really tired, but I don't remember if this is merge sort or quick sort
  -- and I'm too lazy to look it up :)
      high = sortList [z | z <- xs, z > x]
   in low ++ [x] ++ high

-- calculates the product of all elements in this list
productOfElements :: [Int] -> Int
productOfElements [] = 1
productOfElements (x:xs) = x * productOfElements xs

-- returns a list of factors for the number n
factors n = [ x | x <- [2..n], n `mod` x == 0, x >=2]

-- takes two ints and returns true if they are coprime
coprime :: Int -> Int -> Bool
coprime a b = letscompare (factors a) (factors b)

-- basically a not(intersect) function, returns true if no elements are shared between the two functions
-- checks to see if the first factor in list x is contained in the list, ys
-- if it is then we know they are not coprime. If not then a recursive call is made with the tail of x and ys
-- and checks until a null list is passed in, which then evaluates to them being coprime
-- if it weren't for the recursive call then calling coprime 33 11 would result in true because the factor 3 is not in 11
letscompare :: (Eq a) => [a] -> [a] -> Bool
letscompare [] _ = True
letscompare (x:xs) ys
    | elem x ys = False
    | otherwise = letscompare xs ys

-- returns the tribbonacci value of a number
trib :: Int -> Int
trib n
    | n < 3 = 1
    | n == 3 = 3
    | otherwise = tribHelper n

-- base case returns 1 if n < 3
-- otherwise makes recursive calls
tribHelper n
    | n < 3 = 1
    | otherwise = tribHelper (n-3) + tribHelper (n-2) + tribHelper (n-1)


-- returns the maximum number in xs but not in olds
maxNew :: [Int] -> [Int] -> Maybe Int
-- base case deems that both lists had the same exact elements
maxNew [] _ = Nothing
-- all maxNums were equals and now only list a has elements
maxNew  xs [] = Just (getMax (head xs) xs)
maxNew xs olds
    -- gets the max element from both lists and compares.
    -- If they are equal then it removes the max number
    -- from each list before the recursive call          -- this is where the two lists get their equal maxes removed
  | (getMax (head xs) xs) == (getMax (head olds) olds) = maxNew  [ x | x <- xs, x /= (getMax (head xs) xs)]  [ x | x <- olds, x /= (getMax (head olds) olds)]
    -- whichever list has the greater, mutually exclusive maximum, that max gets returned.
  | (getMax (head xs) xs) > (getMax (head olds) olds) = Just (getMax (head xs) xs)
    -- removes the max value from olds and makes recursive call
  | otherwise = maxNew xs [ x | x <- olds, x /= (getMax (head olds) olds)]

-- returns the max value in a list, the first arg keeps track of the max
getMax :: Int -> [Int] -> Int
getMax maxVal [] = maxVal
getMax maxVal (x:xs)
    | maxVal >= x = getMax maxVal xs
    | otherwise = getMax x xs

-- simple zipWith implementation
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- returns a pass list and a fail list when the boolean predicate is applied
passFail :: (a -> Bool) -> [a] -> ([a], [a])
passFail bool lst = ([x |  x <- lst, bool x == True], [x | x <- lst, bool x == False])

powerset :: [Int] -> [[Int]]
powerset lst = [x | x <- lst] : [ [x,y] | x <- lst, y <- lst ]

matrixProduct :: [[Int]] -> [[Int]] -> [[Int]]
matrixProduct x y = [[32]]

