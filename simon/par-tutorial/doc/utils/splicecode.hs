import System.Environment
import System.FilePath
import Data.Maybe
import System.Directory
import System.IO

main = mapM_ doFile =<< getArgs

doFile :: FilePath -> IO ()
doFile f = do
  s <- readFile f
  tmpdir <- getTemporaryDirectory
  (tmpf, tmp) <- openTempFile "." "splicecode.tex"
  let
    proc (Left  l) = hPutStrLn tmp l
    proc (Right (hs,ident)) = do
       src <- readFile hs
       hPutStr tmp (head [ s | (ident',s) <- mungehs (lines src)
                             , ident' == ident ])
  --
  mapM_ proc (mungetex (lines s))
  hClose tmp
  renameFile f (f <.> "old")
  renameFile tmpf f


mungetex :: [String] -> [Either String (FilePath,String)]
mungetex ls = case ls of
  [] -> []
  open : begin : rest | Just (f,ident) <- isOpenSnippet open
    -> Left open : Left begin : Right (f,ident) : dropsnip rest
  other : rest
    -> Left other : mungetex rest
 where
  dropsnip [] = []
  dropsnip (end:close:ls)
     | isCloseSnippet close = Left end : Left close : mungetex ls
  dropsnip (other:ls)
     = dropsnip ls

  isOpenSnippet :: String -> Maybe (FilePath,String)
  isOpenSnippet s = case s of
      '%' : ' ' : '<' : '<' : s -> Just (file,ident)
         where (file,':':ident) = break (==':') s
      _other -> Nothing
  
  isCloseSnippet :: String -> Bool
  isCloseSnippet s = case s of
      '%' : ' ' : '>' : '>' : [] -> True
      _other -> False



mungehs :: [String] -> [(String, String)]
mungehs [] = []
mungehs (l:ls)
  | Just f <- isOpenSnippet l = slurp f [] ls
  | otherwise                 = mungehs ls
 where
  slurp f xs [] = [(f,unlines (reverse xs))]
  slurp f xs (l:ls)
    | isCloseSnippet l = (f, unlines (reverse xs)) : mungehs ls
    | otherwise        = slurp f (l:xs) ls

  isOpenSnippet :: String -> Maybe String
  isOpenSnippet s = case s of
      '-' : '-' : ' ' : '<' : '<' : ident -> Just ident
      _other -> Nothing
  
  isCloseSnippet :: String -> Bool
  isCloseSnippet s = case s of
      '-' : '-' : ' ' : '>' : '>' : [] -> True
      _other -> False
