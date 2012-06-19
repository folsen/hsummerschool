wc :: String -> Int
wc = length . words

lc :: String -> Int
lc = length . lines

-- sentences :: String -> [String]
-- sentences [] = []
-- sentences (x:xs)
--   | x `elem` ".!?" = 

sc :: String -> Int
-- sc = length . filter (\c -> c == '.' || c == '!' || c == '?')
sc = length . filter (`elem` ".!?")

awl :: String -> Float
awl = avg . map wc . lines

avg :: [Int] -> Float
avg x = fromIntegral (sum x) / fromIntegral (length x)


