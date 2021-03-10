-- Зад.1. Използвайте foldr или foldl, за да напишете следните функции:
-- minimum'/maximum'
-- reverse'
-- length'
-- all'/any'
-- append'
-- replicate'

minimum' (x:xs) = foldl min x xs
maximum' (x:xs) = foldl max x xs

-- reverse' lst = foldl (\res el -> el:res) [] lst
-- all' p lst = foldr (\el res -> p el && res) True lst
-- any' :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Bool
-- any' p lst = foldr (\el res -> p el || res) False lst
-- length' lst = foldl (\res _ -> res + 1) 0 lst
append' lst1 lst2 = foldr (:) lst2 lst1
replicate' n x = foldl (\res _ -> x:res) [] [1..n]

--Зад.2. Използвайте list comprehension, за да напишете следните функции:
-- брой/сума на делителите на дадено число
-- проверка дали дадено число е просто
-- descartes - декартово произведение на два списъка
countDivisors :: Int -> Int
countDivisors n = length [d | d<-[1..n-1], n `mod` d == 0]

isPrime :: Int -> Bool 
isPrime n = null [x|x<-[2..n-1], n `mod` x == 0]

descartes:: [a] -> [b] -> [(a, b)]
descartes lst1 lst2 = [(x, y) | x<-lst1, y<-lst2]

-- Зад.3. Да се генерира безкрайния списък primes от прости числа:
primes:: [Int]
primes = filter isPrime [1..]

-- Зад.5. Да се генерира безкраен списък, който съдържа всички наредени двойки от естествени числа.
allNaturals = [(x-y,y)| x<-[0..], y<-[0..x]]

-- Зад.6. Да се генерира безкраен списък, който съдържа всички Питагорови тройки: наредени тройки от естествени числа, които могат да бъдат страни на правоъгълен триъгълник
pytTriples = [(a, b, c)| c<-[5..], b<-[1..c], a<-[1..b], a ^ 2 + b ^ 2 == c ^ 2]

-- Зад.7. Да се напише функция compress, която по списък от стойности връща списък от наредени двойки от вида (<стойност>, <брой последователни срещания>):
compress [] = []
compress lst@(x:_) = (x, length (takeWhile (== x) lst)):compress (dropWhile (== x) lst)

compress' [] = []
compress' lst = (head lst, length heads) : compress' rest
    where (heads, rest) = span (\ x -> x == head lst) lst

-- Зад.8. Да се напише функция maxRepeated, която по списък от стойности връща дължината на най-дългия подсписък, съставен от еднакви стойности:

maxRepeated lst = foldl (\ res (_, n) -> max n res) 0 (compress lst)
maxRepeated' lst = maximum (map snd (compress lst))
maxRepeated'' lst = maximum [n | (_, n)<- compress lst]

-- Зад.9. Да се напише функция makeSet, която по даден списък връща всички негови уникални елементи (редът им няма значение):
makeSet lst = foldr (\ el res -> if el `elem` res then res else el:res) [] lst

-- Зад.10. Да се напише функция histogram, която за всяка уникална стойност от даден списък връща списък от наредени двойки от вида (<стойност>, <общ брой срещания>):
histogram lst = [(elem, count elem)| elem<- makeSet lst]
    where count el = length [x | x<-lst, x == el]

-- Зад.11. Да се напише функция maxDistance, която получава списък от точки (наредени двойки (Double, Double)) и връща дължината на най-дългата отсечка между някои две от тях.

maxDistance lst = maximum [dist p1 p2 | p1<-lst, p2<-lst]
    where dist (x1,y1) (x2,y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
