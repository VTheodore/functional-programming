import Data.Maybe
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- data Maybe a = Nothing | Just a 

balanced :: Tree a -> Bool 
balanced Empty = True 
balanced (Node _ left right) = balanced left && balanced right && abs (height left - height right) <= 1
    where height tree = undefined 

balanced' :: Tree a -> Bool 
balanced' Empty = True 
balanced' t = fst $ helper t
    where helper :: Tree a -> (Bool, Int)
          helper Empty = (True, 0)
          helper (Node _ left right)
            | leftBal && rightBal && abs (leftHeight - rightHeight) <= 0 = (True, h)
            | otherwise = (False, h)
                where (leftBal, leftHeight) = helper left
                      (rightBal, rightHeight) = helper right
                      h = 1 + max leftHeight rightHeight



balanced'' :: Tree a -> Bool 
balanced'' t = helper t /= (-1)
    where helper :: Tree a -> Int
          helper Empty = 0 
          helper (Node _ left right)
            |leftHeight /= (-1) && rightHeight /= (-1) && abs (leftHeight - rightHeight) <= 1 = bigger + 1
            |otherwise = -1
              where leftHeight = helper left
                    rightHeight = helper right
                    bigger = max leftHeight rightHeight

balanced''' :: Tree a -> Bool
--balanced''' t = case helper t of Just _ -> True
--                                 Nothing -> False 
balanced''' t = isJust $ helper t
    where helper :: Tree a -> Maybe Int
          helper Empty = Just 0
          helper (Node _ left right) =
            case (helper left, helper right) of (Just leftH, Just rightH) -> Just (1 + max leftH rightH)
                                                _ -> Nothing 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing 
safeTail (x:xs) = Just xs

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix lst1 lst2
    |lst1 `isPrefixOf` lst2 = Just $ drop (length lst1) lst2
    |otherwise = Nothing
        where isPrefixOf = undefined 


-- Зад.5. Да се напише функция maxSumPath, която приема за аргумент двоично дърво с числа във възлите и намира максималната сума на числата по някой път от корен до листо.
maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0;
maxSumPath (Node val left right) = val + max (maxSumPath left) (maxSumPath right)

-- Зад.6. Да се напише функция prune, която по дадено двоично дърво t връща ново дърво t', което представлява t, в което всички листа са премахнати.

testTree :: Tree Int
testTree = Node 5
                (Node 6 Empty Empty)
                (Node (-10)
                        (Node 2 Empty Empty)
                        Empty)

pruned :: Tree Int
pruned = Node 5
        Empty
        (Node (-10)
            Empty
            Empty)


instance (Eq a ) => Eq (Tree a) where
    Empty == Empty = True
    (Node val1 left1 right1) == (Node val2 left2 right2) = val1 == val2 && left1 == left2 && right1 == right2
    _ == _ = False

prune :: (Eq a) => Tree a -> Tree a
prune Empty = Empty
prune (Node val left right)
    |left == Empty && right == Empty = Empty 
    |otherwise = Node val (prune left) (prune right)

-- Зад.7. Да се напише функция bloom, която по дадено двоично дърво t връща ново дърво t', което представлява t, в което на всички листа са добавени по два наследника - нови листа. Стойността в тези нови листа да е същата, като в оригиналното листо, от което са излезли.

bloomed :: Tree Int
bloomed = Node 5
               (Node 6
                     (Node 6 Empty Empty)
                     (Node 6 Empty Empty))
               (Node (-10)
                       (Node 2
                             (Node 2 Empty Empty)
                             (Node 2 Empty Empty))
                       Empty)

bloom :: (Eq a) => Tree a -> Tree a
bloom Empty = Empty
bloom t@(Node val left right)
    | left == Empty && right == Empty = Node val t t 
    | otherwise = Node val (bloom left) (bloom right)



-- Зад.8. Да се имплементират стандартните ротации на двоични дървета:

rotateLeft, rotateRight :: Tree a -> Tree a

rotateRight (Node p a (Node q b c)) = Node q (Node p a b) c 
rotateRight t = t

rotateLeft (Node q (Node p a b) c) =  Node p a (Node q b c)
rotateLeft t = t

type Graph = [[Int]]

testGraph :: [[Int]]
testGraph = [[1, 2],
            [2, 5],
            [3],
            [5],
            [],
            [4]]

graphSize :: Graph -> Int 
graphSize = length

neighbours :: Int -> Graph -> [Int]
neighbours u g = g !! u

-- DFS
-- colors = replicate n White
-- foreach u in [0..n-1]
--     if (colors[u] == White)
--          dfsVisit u g

-- DFSVisit
-- 1. colors[u] = Gray
-- 2. foreach v in neighbs u
-- 3.   if colors[v] == White
-- 4.     DFSVisit v g
-- 5.   else if colors[v] == Gray
-- 6.     има цикъл => няма топологично сортиране
-- 7. colors[u] = Black
-- 8. добавяме връх u отдясно-наляво в списъка със топологично сортирани върхове
-- Същият проблем -> същото решение :)

-- foldl (+) [1,2,3]
-- (((0 + 1) + 2) + 3)
update :: Int -> a -> [a] -> [a]
update idx el lst = take idx lst ++ (el:drop (idx + 1) lst)

data Colors = White | Gray | Black deriving (Eq, Show)

type State = [Colors]

dfs :: Graph -> State
dfs g = foldl helper (replicate n White) [0..n-1]
    where n = graphSize g
          helper :: State -> Int -> State
          helper colors u
            | colors !! u == White = dfsVisit u colors
            | otherwise  = colors
          dfsVisit :: Int -> State -> State
          dfsVisit u colors = let colors' = foldl helper' (update u Gray colors) (neighbours u g)
                              in update u Black colors
            where  helper' :: State -> Int -> State
                   helper' colors v
                    | colors !! v == White = dfsVisit v colors
                    | otherwise = colors 