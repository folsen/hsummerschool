import Data.Char

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show, Ord)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Eq a => Eq (Tree a) where
  Empty == Empty = True
  Empty == (Node _ _ _) = False
  (Node _ _ _) == Empty = False
  (Node l1 a1 r1) == (Node l2 a2 r2) = l1 == l2 && a1 == a2 && r1 == r2

-- instance Ord a => Ord (Tree a) where
--   compare Empty Empty = EQ
--   compare Empty (Node _ _ _) = LT
--   compare (Node _ _ _) Empty = GT
--   compare t1@(Node l1 a1 r1) t2@(Node l2 a2 r2)
--     | size t1 < size t2 = LT
--     | size t1 > size t2 = GT
--     | otherwise = compare a1 a2

--instance Eq a => Ord (Tree a) where
--  compare t1 t2 = compare (size t1) (size t2)
--

class Sizeable a where
  size :: a -> Integer

instance Sizeable Int where
  size _ = 1

instance Sizeable Char where
  size _ = 1

instance Sizeable Bool where
  size _ = 1

instance Sizeable a => Sizeable (Maybe a) where
  size Nothing  = 0
  size (Just x) = 1

instance Sizeable [a] where
  size [] = 0
  size l  = fromIntegral (length l)

instance Sizeable (Tree a) where
  size Empty = 0
  size (Node l a r) = 1 + size l + size r

class Hashable a where
  hash :: a -> Integer

instance Hashable Int where
  hash x = fromIntegral x

instance Hashable Char where
  hash x = fromIntegral (digitToInt x)

instance Hashable Bool where
  hash True  = 1
  hash False = 0

instance Hashable a => Hashable (Maybe a) where
  hash Nothing  = 0
  hash (Just x) = 1 + hash x

instance Hashable a => Hashable [a] where
  hash [] = 0
  hash xs = sum $ zipWith (\x p -> (1 + hash x) * (10 ^ p)) xs [0..]


-- size :: Tree a -> Integer
-- size Empty = 0
-- size (Node l _ r) = 1 + size l + size r

tree :: [a] -> Tree a
tree [] = Empty
tree (x:xs) = Node (tree l) x (tree r)
  where 
    mid = length xs `div` 2
    l = take mid xs
    r = drop mid xs

member :: (Eq a) => a -> Tree a -> Bool
member _ Empty = False
member x (Node l e r) = x == e || member x l || member x r

searchTree :: (Ord a) => [a] -> Tree a
searchTree [] = Empty
searchTree (x:xs) = Node (searchTree smaller) x (searchTree larger)
  where
    smaller = [y | y <- xs, y < x]
    larger  = [y | y <- xs, y >= x]

memberS :: (Ord a) => a -> Tree a -> Bool
memberS _ Empty = False
memberS x (Node l e r)
  | x == e = True
  | x < e = memberS x l
  | otherwise = memberS x r

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node l e r) = inOrder l ++ [e] ++ inOrder r



-- []        0
-- [1]       1
-- [2]       2
-- [1,1]     3
-- [3]
-- [2,1]
-- [1,2]
-- [1,1,1]
-- [4]
