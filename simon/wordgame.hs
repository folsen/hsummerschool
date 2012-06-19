import System.IO
import System.Random
import Control.Concurrent
import Control.Monad

data Event = NewChar Char | Tick
type EVar = MVar Event

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  
  ev <- newEmptyMVar
  forkIO . forever $ readInput ev
  forkIO $ waitASecond ev 0
  gameLoop ev 0 0 ""

readInput :: EVar -> IO ()
readInput ev = do
  c <- getChar
  putMVar ev (NewChar c)

putString :: Int -> String -> IO ()
putString n s = do
  putStr (replicate n '\8')
  putStr (replicate n ' ')
  putStr (replicate n '\8')
  putStr s

-- Go from 10^6 to 100 over 60 seconds
-- Linearly: 10^6 - (999900)/60*t
waitASecond :: EVar -> Int ->  IO ()
waitASecond ev t = do
  let delay = if t <= 60
                then (10^6 - 999900 `div` 60*t)
                else 100
  threadDelay delay
  putMVar ev Tick
  waitASecond ev (t+1)
  
gameLoop :: EVar -> Int -> Int -> String -> IO ()
gameLoop ev n score str
  | length str == 10 = putString n ("Game Over!\nYour score was: " ++ show score ++ "\n")
  | otherwise  = do
      putString n str
      event <- takeMVar ev
      case event of
        NewChar c -> let ls = length str
                         ns = filter (/= c) str
                         newscore = score + (ls - length ns)
                     in gameLoop ev ls newscore ns
        Tick -> do 
                  nc <- randomChar
                  gameLoop ev (length str) score (nc : str)

randomChar :: IO Char
randomChar = randomRIO ('a', 'z')
