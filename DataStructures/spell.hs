import System.Environment
import Data.Set hiding (filter, map, null, fromList)
import Data.Char
import Trie
import Test.HUnit

main :: IO ()
main = do
  args <- getArgs
  spellCheckFiles (args !! 0) (args !! 1)
  runTestTT tests
  return ()

type Dictionary = TrieSet Char -- [String]

sampleDict :: Dictionary
sampleDict = fromList [("hello", ()), ("i", ()), ("am", ()), ("here", ())]

-- | Words in string but not in dictionary
spellCheck :: String -> Dictionary -> [(Integer, Int, String)]
spellCheck s dict = filter (\(i,j,w) -> notTrieSetMember (map toLower w) dict) ((toWords . toLines) s)

toLines :: String -> [(Integer, String)]
toLines s = zip [1..] (lines s)

toWords :: [(Integer, String)] -> [(Integer, Int, String)]
toWords lines = [(i, j, w) | (i,l) <- lines, (w,j) <- words' l 0]

words'     :: String -> Int -> [(String, Int)]
words' s i = case dropWhile {-partain:Char.-}isSpace s of
              "" -> []
              s' -> (w, idx): words' s'' (idx+length w)
                where 
                  (w, s'') = break {-partain:Char.-}isSpace s'
                  idx =  i+(substring 0 w s)

substring :: Int -> String -> String -> Int
substring x s1 s2
  | null dropped = -1
  | taken = x
  | otherwise = substring (x+1) s1 s2
  where 
    taken = take (length s1) dropped == s1
    dropped = drop x s2


spellCheckFiles :: FilePath -> FilePath -> IO [(Integer, Int, String)]
spellCheckFiles input dict =
  do
    inputTxt <- readFile input
    dictTxt <- readFile dict
    let incorrectWords = spellCheck inputTxt (fromList' $ lines dictTxt)
    mapM_ putStrLn (map show incorrectWords)
    return incorrectWords


test1 = TestCase (assertEqual "spellcheck empty words should be empty list" (spellCheck "helo" sampleDict) [])
test2 = TestCase (do
                    words <- (spellCheckFiles "./input.txt" "/usr/share/dict/american-english")
                    let real = [(1,0,"Hello!"),
                                (2,21,"computer?"),
                                (3,0,"Cahn"),
                                (3,9,"spellcheck"),
                                (3,25,"prooperly"),
                                (3,35,"foor"),
                                (3,40,"me?")]
                    assertEqual "check files" words real)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
