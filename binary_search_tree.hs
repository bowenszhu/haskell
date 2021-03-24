data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

t = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))

occurs :: Ord a => a -> Tree a -> Bool
occurs _ Empty        = False
occurs x (Leaf y    ) = if compare x y == EQ then True else False
occurs x (Node l n r) = if compare x n == EQ
  then True
  else if compare x n == LT then occurs x l else occurs x r

size :: Tree a -> Int
size Empty        = 0
size (Leaf _    ) = 1
size (Node l _ r) = 1 + size l + size r

is_balanced :: Tree a -> Bool
is_balanced Empty    = True
is_balanced (Leaf _) = True
is_balanced (Node l _ r) =
  abs (size l - size r) <= 1 && is_balanced l && is_balanced r

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Leaf x    ) = [x]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

balance :: [a] -> Tree a
balance []  = Empty
balance [x] = Leaf x
balance l =
  let m     = (length l) `div` 2
      value = l !! m
      left  = balance (take m l)
      right = balance (drop (m + 1) l)
  in  Node left value right
