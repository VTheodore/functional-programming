-- 1. Statically vs Dynamically typed languages

-- 2. Function signatures
-- in scheme (define a 5)

-- int a = 5;
a :: Int 
a = 5

mySum :: Int -> Int -> Int 
mySum x y = x + y

-- voidFunction :: Int -> IO () -- () == void

-- 3. GHCi features

-- 4. Pattern matching
-- 4.1 Demo with factorial/fibonacci

-- (define (fact n)
--  (if (= n 0)
--  1
--  (* n (fact (- n 1))))

factorial :: Int -> Int 
factorial n = if n == 1 then 1 else n * factorial (n - 1)

fact :: Int -> Int 
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- 4.2 Sum all elements of a list

-- if/then/else
-- head/tail - car/cdr
sumElements :: [Int] -> Int 
sumElements xs = if null xs then 0 else head xs + sumElements (tail xs)

-- empty list pattern - []
sumElements' :: [Int] -> Int 
sumElements' [] = 0
sumElements' xs = head xs + sumElements' (tail xs)

-- non-empty list pattern - (x:xs)
-- x is the first element
-- the rest are found in xs
sumElements'' :: [Int] -> Int 
sumElements'' [] = 0
sumElements'' (x:xs) = x + sumElements'' xs

-- 4.3 Length of a list using sum and list comprehension

-- if we don't care about the current element
-- replace with _
listLength :: [a] -> Int  
listLength [] = 0;
listLength (_:xs) = 1 + listLength xs

-- change all of the elements int the list with 1
-- and then find the sum
listLength' :: [a] -> Int 
listLength' xs = sum [1 | _ <- xs]

-- Othe list comprehensions
evenDoubled = [x * 2 | x <- [1..10], even x]

-- useful functions for numbers
-- guard - complex patterns that cannot be captured in the regular pattern matching
-- such as n < 0
-- works the same way as cond in scheme
countDigits :: Int -> Int 
countDigits n
    |n < 0 = countDigits (-n)
    |n < 10 = 1
    |otherwise = 1 + countDigits (n `div` 10)
--  ^ guards