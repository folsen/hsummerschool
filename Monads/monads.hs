import Control.Monad.State
import Data.Map hiding (filter, foldr)
import Prelude hiding (lookup)

sumS :: Num a => [a] -> State a a
sumS []     = do
  sum <- get
  return sum
sumS (x:xs) = do
  sum <- get
  put $ x + sum
  sumS xs

evalSumS :: Num a => [a] -> a
evalSumS xs = evalState (sumS xs) 0

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show)

decorate :: Tree a -> State Int (Tree (Int, a))
decorate Empty = return Empty
decorate (Node l e r) = do
  lt   <- decorate l
  nmbr <- get
  put (nmbr + 1)
  rt   <- decorate r
  return $ Node lt (nmbr, e) rt

evalDecorate :: Tree a -> Tree (Int, a)
evalDecorate t = evalState (decorate t) 1

sampleTree = Node (Node Empty 'b' Empty) 'a' (Node (Node Empty 'd' Empty) 'c' Empty)

leastNatural :: [Integer] -> State [Integer] Integer
leastNatural [] = do
  s <- get
  return (head s)
leastNatural (x:xs) = do
  s <- get
  let s2 = filter (/= x) s
  put s2
  leastNatural xs


-- Map Integer Bool
-- [Integer]
-- Current Position


findNextIndex :: Integer -> Map Integer Bool -> Integer
findNextIndex i m = case lookup i m of
  Nothing    -> i
  Just False -> findNextIndex (i + 1) m
  Just True  -> i

leastNat :: [Integer] -> Integer -> State (Integer, Map Integer Bool) Integer
leastNat []  n    = get >>= return . fst
leastNat (x:xs) n = do
   when (0 <= x && x <= n)
        (get >>= put . (getNewState x n))
   leastNat xs n
 
getNewState :: Integer -> Integer -> 
               (Integer, Map Integer Bool) -> 
               (Integer, Map Integer Bool)
getNewState x n (i, m)
  | x == i = (findNextIndex i m', m)
  | otherwise = (i, m')
  where m' = insert x False m

initState :: Integer ->  Map Integer Bool
initState n = foldr (\i m -> insert i True m) empty [0..n] 
        
evalLeast :: [Integer] -> Integer
evalLeast xs = evalState (leastNat xs n) (0, initState n)
  where
    n = fromIntegral $ length xs



