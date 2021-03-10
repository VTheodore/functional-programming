module Ex12 (specialSort) where
import Data.List(nub, maximumBy, sortOn)
import Data.Ord(comparing)

-- Зад.12*. Да се напише функция specialSort, която приема като параметър списък от списъци и го сортира относно най-често срещания елемент във всеки от вътрешните списъци. Ако има няколко най-често срещани елемента, да се избира най-големия от тях:
ex = ["moo", "bee", "eve", "abracadabra", "abcdefg", "mama", "z"]
con = foldl (foldl (flip (:))) [] ex

-- Eq - за типове, чиито стойности могат да бъдат сравнявани с == или /=
-- Ord - за типове, чиито стойности могат да бъдат сравнявани с <, <=, > или >=
-- Show, Read
-- Integral - множество от целочислени типове (Int,Integer)
-- Floating - множество от дробни числа (Float, Double)
-- Num - за всички числа

mostFrequent :: (Eq a) => [a] -> a
mostFrequent lst = fst $ maximumBy (comparing snd) (histogram lst)

histogram :: (Eq a) => [a] -> [(a, Int)]
histogram lst = [(el, count el lst)| el<-uniques lst]
    where count elem xs = length $ filter (== elem) xs

uniques :: (Eq a) => [a] -> [a]
uniques = nub

specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort = sortOn mostFrequent

-- Зад.1 Да се напише алгебричен тип данни Parity, който обозначава четността на дадено число. Да се реазлизират класовете Eq, Ord, Show, Read
-- Бел.авт.: Eq е необходимо, за да използваме стойности от този тип в pattern matching - все пак той е синтактична захар за if x == 5 then ... else ...

-- data Bool = True | False

data Parity = Even | Odd

instance Eq Parity where
    Even == Even = True 
    Odd == Odd = True 
    _ == _ = False 

parity :: Integral a => a -> Parity
parity x = if even x then Even else Odd

isEven :: Parity -> String
isEven Even = "yeaah"
isEven _ = "naaah"

isEven' :: Integral a => a -> String
isEven' = isEven . parity  

data IntTree = EmptyTree | Node Int IntTree IntTree

treeSum :: IntTree -> Int
treeSum EmptyTree = 0
treeSum (Node val left right) = val + treeSum left + treeSum right