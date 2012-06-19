main :: IO ()
main = print "Hej!"

prod :: [Int] -> Int
prod = foldr (*) 1

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

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
largest = foldr go Nothing
  where
    go :: Int -> Maybe Int -> Maybe Int
    go x m
      | Just x > m = Just x
      | otherwise = m

smallest :: [Int] -> Maybe Int
smallest = foldr go Nothing
  where
    go :: Int -> Maybe Int -> Maybe Int
    go x Nothing  = Just x
    go y (Just x)
      | x < y     = Just x 
      | otherwise = Just y

fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib' n = fibs !! n
  where
    fibs = reverse $ foldr (\x lst -> (head lst) + (head (tail lst)) : lst ) [1,0] [1..10]

fib'' :: Int -> Integer
fib'' n = fibs !! n
  where
    fibs = unfoldr (\(a,b) -> Just (a+b, (b, a+b))) (0,1)


fac :: Integer -> Integer
fac n = foldr (*) 1 (efromTo 1 n)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr gen s = case gen s of
                  Nothing    -> []
                  Just (x,r) -> x : unfoldr gen r

efromTo :: Integer -> Integer -> [Integer]
efromTo l u = unfoldr (\ c -> if c > u then Nothing else Just (c, c+1)) l
