-- | This is the most awesome module comment of all!
module Ex6lib where

-- | Calculate the product of a list of numbers
prod :: [Int] -> Int
prod = foldr (*) 1

-- | Checks if all bools in a list is true
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- | Checks if all are false.
allFalse :: [Bool] -> Bool
-- allFalse = allTrue . map not
allFalse = foldr ((&&) . not) True

decAll :: [Int] -> [Int]
decAll = map (subtract 1)

convertIntBool :: [Int] -> [Bool]
convertIntBool = foldr ((:) . (0 ==)) []
-- convertIntBool = map (== 0)

-- map f = foldr ((:) . f) []

pairUp :: [a] -> [b] -> [(a,b)]
pairUp = zip

takePrefix :: Int -> [a] -> [a]
takePrefix = take

dropPrefix :: Int -> [a] -> [a]
dropPrefix = drop

member :: Eq a => [a] -> a -> Bool
member xs x = foldl (\b e -> b || e == x) False xs
-- member xs x = foldl ((. (x ==)) . (||)) False xs

-- select :: [a] -> Int -> Maybe a
-- select xs i = foldl (\m (n,e) -> if i == n then Just e else m ) Nothing $ foldl (\l m -> (length l, m):l) [] xs 
-- 
-- select xs i = foldl ((`ap` snd) . (. fst) . flip (flip . (. Just) . if' . (i ==))) Nothing $ foldl (flip =<< ((:) .) . (,) . length) [] xs 
-- select = flip (flip foldl Nothing . (((`ap` snd) . (. fst)) .) . flip . ((flip . (. Just) . if') .) . (==)) . foldl (flip =<< ((:) .) . (,) . length) []
-- 
-- select :: [a] -> Int -> Maybe a
-- select [] _ = Nothing
-- select (x:_) 0 = Just x
-- select (_:xs) i = select xs (i-1)

select :: [a] -> Int -> Maybe a
select = foldr go (const Nothing)
  where
   go :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
   go x r = \ i -> if i == 0 then Just x else r (i-1)

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



