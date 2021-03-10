{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име: Теодор Везенков
  ФН: 62467
  Специалност: СИ
  Курс: 2
  Административна група: Е
  Начален час на контролното: 08:00
--}

module K3_62467 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a

                          } deriving (Show,Read)
sampleTree :: Tree Char
sampleTree = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))

exTre = Node 'a' (Node 'b' EmptyTree EmptyTree) (Node 'c' EmptyTree EmptyTree)


treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords t = [x | x<-helper t "", x /= ""]
    where helper :: Tree Char -> String -> [String]
          helper EmptyTree _ = [""]
          helper (Node val EmptyTree EmptyTree) curr = [curr ++ [val]]
          helper (Node val left right) curr = helper left (curr ++ [val]) ++ helper right (curr ++ [val])

mapsTo :: Integral t => (t -> t) -> t -> t -> (t,t)
mapsTo f a b = if a <= b then (minimum res, maximum res) else undefined 
    where res = [f x|x<-[a..b]]
