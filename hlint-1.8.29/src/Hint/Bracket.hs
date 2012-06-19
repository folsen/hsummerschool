{-# LANGUAGE ViewPatterns #-}
{-
Raise an error if you are bracketing an atom, or are enclosed be a list bracket

<TEST>
-- expression bracket reduction
yes = (f x) x -- @Warning f x x
no = f (x x)
yes = (foo) -- foo
yes = (foo bar) -- @Warning foo bar
yes = foo (bar) -- @Error bar
yes = foo ((x x)) -- @Error (x x)
yes = (f x) ||| y -- @Warning f x ||| y
yes = if (f x) then y else z -- @Warning if f x then y else z
yes = if x then (f y) else z -- @Warning if x then f y else z
yes = (a foo) :: Int -- @Warning a foo :: Int
yes = [(foo bar)] -- @Warning [foo bar]
yes = foo ((x y), z) -- @Warning (x y, z)
yes = C { f = (e h) } -- @Warning C {f = e h}
yes = \ x -> (x && x) -- @Warning \x -> x && x
no = \(x -> y) -> z
yes = (`foo` (bar baz)) -- @Warning (`foo` bar baz)
main = do f; (print x) -- @Warning do f print x

-- type bracket reduction
foo :: (Int -> Int) -> Int
foo :: Int -> (Int -> Int) -- @Warning Int -> Int -> Int
foo :: (Maybe Int) -> a -- @Warning Maybe Int -> a
instance Named (DeclHead S)
data Foo = Foo {foo :: (Maybe Foo)} -- @Warning foo :: Maybe Foo

-- pattern bracket reduction
foo (True) = 1
foo ((True)) = 1 -- @Error True

-- dollar reduction tests
no = groupFsts . sortFst $ mr
yes = split "to" $ names -- split "to" names
yes = white $ keysymbol -- white keysymbol
yes = operator foo $ operator -- operator foo operator
no = operator foo $ operator bar
yes = return $ Record{a=b} -- return Record{a=b}

-- $/bracket rotation tests
yes = (b $ c d) ++ e -- b (c d) ++ e
yes = (a b $ c d) ++ e -- a b (c d) ++ e
no = (f . g $ a) ++ e
no = quickCheck ((\h -> cySucc h == succ h) :: Hygiene -> Bool)
foo = (case x of y -> z; q -> w) :: Int

-- backup fixity resolution
main = do a += b . c; return $ a . b
</TEST>
-}


module Hint.Bracket where

import Hint.Type


bracketHint :: DeclHint
bracketHint _ _ x =
    concatMap (\x -> bracket True x ++ dollar x) (childrenBi x :: [Exp_]) ++
    concatMap (bracket False) (childrenBi x :: [Type_]) ++
    concatMap (bracket False) (childrenBi x :: [Pat_]) ++
    concatMap fieldDecl (childrenBi x)


bracket :: (Annotated a, Uniplate (a S), ExactP a, Pretty (a S), Brackets (a S)) => Bool -> a S -> [Idea]
bracket bad = f Nothing
    where
        msg = "Redundant bracket"

        -- f (Maybe (index, parent, gen)) child
        f :: (Annotated a, Uniplate (a S), ExactP a, Pretty (a S), Brackets (a S)) => Maybe (Int,a S,a S -> a S) -> a S -> [Idea]
        f Just{} o@(remParen -> Just x) | isAtom x = err msg o x : g x
        f Nothing o@(remParen -> Just x) | bad = warn msg o x : g x
        f (Just (i,o,gen)) (remParen -> Just x) | not $ needBracket i o x = warn msg o (gen x) : g x
        f _ x = g x

        g :: (Annotated a, Uniplate (a S), ExactP a, Pretty (a S), Brackets (a S)) => a S -> [Idea]
        g o = concat [f (Just (i,o,gen)) x | (i,(x,gen)) <- zip [0..] $ holes o]


fieldDecl :: FieldDecl S -> [Idea]
fieldDecl o@(FieldDecl a b (UnBangedTy c (TyParen _ d)))
    = [warn "Redundant bracket" o (FieldDecl a b (UnBangedTy c d))]
fieldDecl _ = []


dollar :: Exp_ -> [Idea]
dollar = concatMap f . universe
    where
        msg = warn "Redundant $"
        f x = [msg x y | InfixApp _ a d b <- [x], opExp d ~= "$"
              ,let y = App an a b, not $ needBracket 0 y a, not $ needBracket 1 y b]
              ++
              [msg x (t y) |(t, Paren _ (InfixApp _ a1 op1 a2)) <- splitInfix x
              ,opExp op1 ~= "$", isVar a1 || isApp a1 || isParen a1, not $ isAtom a2
              ,let y = App an a1 (Paren an a2)]


-- return both sides, and a way to put them together again
splitInfix :: Exp_ -> [(Exp_ -> Exp_, Exp_)]
splitInfix (InfixApp s a b c) = [(InfixApp s a b, c), (\a -> InfixApp s a b c, a)]
splitInfix _ = []
