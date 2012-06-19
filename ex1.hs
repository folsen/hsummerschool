add :: Integer -> Integer -> Integer
add x y = x + y

double :: Integer -> Integer
double x = x + x

first :: Integer -> Integer -> Integer
first x y = x

cond :: Bool -> Integer -> Integer -> Integer
cond x y z = if x then y else z

twice :: (Integer -> Integer) -> Integer -> Integer
twice f x = f (f x)

infinity :: Integer
infinity = infinity + 1
