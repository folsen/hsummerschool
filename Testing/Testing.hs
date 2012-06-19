import qualified Data.List as L
import Test.QuickCheck
import qualified Data.Set as S
import Control.Applicative

-- sort :: [Int] -> [Int]
-- sort [ ]      = []
-- sort (x : xs) = insert x (sort xs)

insert :: Int -> [Int] -> [Int]
insert x [ ]                  = [x]
insert x (y : ys) 
  | x <= y       = x : y : ys
  | otherwise = y : insert x ys

sortPreservesLength :: [Int] -> Bool
sortPreservesLength xs = length xs == length (sort xs)


sorted :: [Int] -> Bool
sorted [ ]       = True
sorted (x : [ ]) = True
sorted (x : y : ys) = x <= y && sorted (y : ys)

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted xs = sorted (sort xs)

sortIdempotent :: [Int] -> Bool
sortIdempotent xs = sort xs == sort (sort xs)

sortSameDataList :: [Int] -> Bool
sortSameDataList xs = sort xs == L.sort xs

testPoly :: [a] -> Bool
testPoly xs = length xs == 0

dropTwice :: Int -> Int -> [Int] -> Property
dropTwice m n xs = (m >= 0 && n >= 0 && m+n >= 0 && m + n <= maxBound) ==> drop m (drop n xs) == drop (m + n) xs

permutations :: Eq a => [a] -> [[a]]
permutations []    = [[]]
permutations [x]   = [[x]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (L.delete x xs) ]

f `permutes` xs = f xs `elem` permutations xs
sortPermutes xs = sort `permutes` xs

sameElems :: Eq a => [a] -> [a] -> Bool
sameElems orig perm = osInPs && psInOs
  where
    check l1 l2 = foldr (\p b -> b && elem p l1) True l2
    osInPs = check perm orig
    psInOs = check orig perm 

sortPermutes' :: [Integer] -> Bool
sortPermutes' xs = sameElems xs (sort xs)

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x]= [x]
sort xs = merge (sort left) (sort right)
  where
    mid = length xs `div` 2
    left = take mid xs
    right = drop mid xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] r  = r
merge l  [] = l
merge (l:ls) (r:rs) 
  | l <= r = l : merge ls (r:rs)
  | otherwise = r : merge (l:ls) rs

newtype EvenList a = Even [a]
  deriving (Eq, Ord, Show, Read)

instance (Arbitrary a, Integral a) => Arbitrary (EvenList a) where
  arbitrary = fmap Even (arbitrary `suchThat` (\xs -> not (null xs) &&  all even xs))

instance (Arbitrary a, Ord a) => Arbitrary (S.Set a) where
  arbitrary = fmap S.fromList arbitrary

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
      gen :: Arbitrary a => Int -> Gen (Tree a)
      gen 0 = pure Leaf <*> arbitrary
      gen n = let subtree = gen (n `div` 2)
                  sub     = oneof [pure Leaf <*> arbitrary, subtree]
              in pure Node <*> sub <*> sub

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) =  size l + size r

flatten :: Tree a -> [a]
flatten (Leaf n)   = [n]
flatten (Node l r) = flatten l ++ flatten r

prop_sameLength :: Tree Int -> Bool
prop_sameLength t = size t == length (flatten t)

rev :: Tree a -> Tree a
rev (Leaf x)   = Leaf x
rev (Node l r) = Node (rev r) (rev l)

prop_revTwice :: Tree Integer -> Bool
prop_revTwice t = (rev . rev) t == t

main :: IO ()
main = do
  quickCheck prop_revTwice
  quickCheck prop_sameLength
  quickCheck sortEnsuresSorted
  quickCheck sortPreservesLength
  quickCheck sortIdempotent
  quickCheck sortSameDataList
  quickCheck dropTwice
  quickCheck sortPermutes'
