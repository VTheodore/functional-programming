import Prelude

sumNew :: [Int] -> Int 
sumNew [] = 0
sumNew (x:xs) = x + sumNew xs

--type String = [Char]

data List a = EmptyList | Cons a (List a) deriving Show
--data StringList = EmptyStringList | StringCons String StringList deriving Show

exampleList :: List Int
exampleList = Cons 1 (Cons 2 (Cons 3 EmptyList))

stringList :: List String 
stringList = Cons "abc" (Cons "def" (Cons "ghi" EmptyList))

sumList :: List Int -> Int 
sumList EmptyList = 0
sumList (Cons x xs) = x + sumList xs

-- премахваме първото срещане на даден елемент от списъка
removeList :: Int -> List Int-> List Int
removeList _ EmptyList = EmptyList
removeList elem (Cons x xs)
    |elem == x = xs
    |otherwise = Cons x (removeList elem xs)

-- връща индекс на първото срещане на дадено число в списък
-- започваме от нула
findList :: Int -> List Int -> Int
findList _ EmptyList = undefined 
findList elem xs = findHelper 0 elem xs

findHelper :: Int -> Int -> List Int-> Int
findHelper _ _' EmptyList = undefined 
findHelper currIdx elem (Cons x xs)
    |elem == x = currIdx
    |otherwise = findHelper (currIdx + 1) elem xs

findList' :: Eq a => a -> List a -> Int 
findList' _ EmptyList = undefined
findList' elem (Cons x xs)
    |elem == x = 0
    |otherwise = 1 + findList' elem xs

type Name = String
type Age = Int
data Person = Person Name Age | PersonWithId Name Int

p1 :: Person
p1 = Person "Pesho" 12

p2 :: Person
p2 = PersonWithId "Pesho" 12

instance Eq Person where 
    (Person n1 a1) == (Person n2 a2) = n1 == n2 && a1 == a2
    (Person n1 _) == (PersonWithId n2 _) = n1 == n2
    (PersonWithId _ id1) == (PersonWithId _ id2) = id1 == id2
    _ == _ = False

-- искаме да вкараме елемент x точно след елемент от списъка xs, който е 
-- по-малък от него
insert:: Ord a => a -> [a] -> [a]
insert _ [] = []
insert x (curr:xs)
    |curr < x = curr:x:xs
    |otherwise = curr:insert x xs

--Enum - [1..20], ['c'..'z']

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

addOne :: Int -> Int
addOne = plus 1

plus :: Int -> Int -> Int
plus a b = a + b