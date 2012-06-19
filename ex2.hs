import Prelude hiding (and, or)

square :: Num a => a -> a
square x = x * x

quad :: Num a => a -> a
quad = square . square

larger :: Ord a => a -> a -> a
larger a b
  | a > b = a
  | otherwise = b

area :: Double -> Double
area r = pi * square r

and :: Bool -> Bool -> Bool
and a b = if a then (if b then True else False) else False

or :: Bool -> Bool -> Bool
or a b
  | a = True
  | b = True
  | otherwise = False

and' :: Bool -> Bool -> Bool
and' a b
  | not a = False
  | otherwise = b

and'' :: Bool -> Bool -> Bool
and'' True b = b
and'' _ _ = False


or' :: Bool -> Bool -> Bool
or' a b
  | a = a
  | otherwise = b

or'' :: Bool -> Bool -> Bool
or'' False b = b
or'' _ _ = True

showDate :: Integer -> Int -> Integer -> String
showDate d m y = show d ++ modifier d ++ " " ++ (months !! (m-1)) ++ " " ++ show  y
  where
    months = ["January", "February", "March"]

modifier :: Integer -> String
modifier x
  | x `mod` 10 == 1 = "st"
  | x `mod` 10 == 2 = "nd"
  | x `mod` 10 == 3 = "rd"
  | otherwise = "th"


prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs

allFalse :: [Bool] -> Bool
allFalse [] = True
allFalse (x:xs) = not x && allFalse xs

decAll :: [Int] -> [Int]
decAll [] = []
decAll (x:xs) = (x-1) : decAll xs

convertIntBool :: [Int] -> [Bool]
convertIntBool [] = []
convertIntBool (x:xs)
  | x == 0 = False : convertIntBool xs
  | otherwise = True : convertIntBool xs

pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] _ = []
pairUp _ [] = []
pairUp (x:xs) (y:ys) = (x,y): pairUp xs ys


takePrefix :: Int -> [a] -> [a]
takePrefix _ [] = []
takePrefix 0 _ = []
takePrefix i (x:xs) = x : takePrefix (i-1) xs

dropPrefix :: Int -> [a] -> [a]
dropPrefix _ [] = []
dropPrefix 0 x = x
dropPrefix i (_:xs) = dropPrefix (i-1) xs


member :: Eq a => [a] -> a -> Bool
member [] _ = False
member (x:xs) e = (e == x) || member xs e

select :: [a] -> Int -> Maybe a
select [] _ = Nothing
select (x:_) 0 = Just x
select (_:xs) i = select xs (i-1)
  

largest :: [Int] -> Maybe Int
largest [] = Nothing
largest [x] = Just x
largest (x:y:xs) = largest (greater:xs)
  where
    greater = if x > y then x else y

largest' :: [Int] -> Maybe Int
largest' [] = Nothing
largest' (x:xs)
  | Just x > largest' xs = Just x
  | otherwise = largest' xs

smallest :: [Int] -> Maybe Int
smallest [] = Nothing
smallest [x] = Just x
smallest (x:xs)
  | Just x < smallest xs = Just x
  | otherwise = smallest xs



