import Control.Applicative
import Control.Monad
import Data.Char
import Debug.Trace

newtype Parser r = P (String -> [(r, String)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P (\s -> [(a, s)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  (P f) <*> (P g) = P (\s -> [ (atob a,s'') | (atob,s') <- f s, (a,s'') <- g s' ])

-- \s0 -> do
--   (a2b, s1) <- f s0
--   (a, s2)   <- g s1
--   return (a2b a, s2)

instance Functor Parser where
  fmap f p = pure f <*> p

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  -- f :: String -> [(a, String)]
  -- g :: a -> (String -> [(b, String)]
  -- (>>=) f g = join (pure g <*> f)
  (>>=) f g = P (\s0 -> [ (b,s) | (a, s1) <- (unwrap f) s0, 
                                  (b, s)  <- ((unwrap . g) a) s1 ])
    where
      unwrap (P a) = a
  return = pure

instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ const [] 
  (P f1) <|> (P f2) = P $ \str -> let f1s = f1 str in
                              if null f1s
                                then f2 str
                                else f1s

runParser :: Parser a -> (String -> [(a, String)])
runParser (P f) = f

eof :: Parser ()
eof = P $ \str -> if str == "" then [((), "")] else []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \str -> satisfy' p str
satisfy' p (y:ys) | p y = [(y, ys)]
satisfy' p _            = []

num :: Parser Int
num = do
  numbers <- many (satisfy isDigit)
  return (transform numbers)


transform :: String -> Int
transform s = read s
