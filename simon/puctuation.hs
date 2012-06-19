import System.IO
import Data.Char
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as B

import BingTranslate as Bing

type Current = TVar String

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  curr <- newTVarIO ""
  forkIO $ render curr "" -- Render thread that checks for changes to string
  forkIO $ correcter curr "" -- Detects changes and writes back corrected result
  takeInput curr -- Read input and append to TVar
  
takeInput :: Current -> IO ()
takeInput curr = do
  c <- getChar
  atomically $ do
    cs <- readTVar curr
    writeTVar curr (cs ++ [c])
  takeInput curr

correcter :: Current -> String -> IO ()
correcter curr old = do 
  new <- atomically $ do
    cs <- readTVar curr
    let new = cs -- transform cs
    when (old == new) retry 
    writeTVar curr new
    return new
  correcter curr new

transform :: String -> String
transform []  = []
transform [x] = [x]
transform (x:y:[]) = x : y : []
transform (x:y:u:xs)
  | x == '.' && y == ' ' = x : y : toUpper u : transform xs
  | otherwise = x : transform (y:u:xs)

render :: Current -> String -> IO ()
render curr old = do
  new <- atomically $ do
    cs <- readTVar curr
    when (cs == old) retry
    return cs
  trans <- translate new 
  putString (length trans) new
  translate new
  render curr new

translate :: String -> IO String
translate str = do
  fromLang <- Bing.detectLanguage str
  trans    <- Bing.translateText str fromLang "ja"
  printf "%s" trans
  return trans
  
putString :: Int -> String -> IO ()
putString n s = do
  putStr (replicate 100 '\8')
  putStr (replicate 100 ' ')
  putStr (replicate 100 '\8')
