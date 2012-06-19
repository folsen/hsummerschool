{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Concept:
    Remove all the lambdas you can be inserting only sections
    Never create a right section with +-# as the operator (they are misparsed)

    Rules:
    fun a = \x -> y  -- promote lambdas, provided no where's outside the lambda
    fun x = y x  -- eta reduce, x /= mr and foo /= symbol
    \x -> y x  -- eta reduce
    ((#) x) ==> (x #)  -- rotate operators
    (flip op x) ==> (`op` x)  -- rotate operators
    \x y -> x + y ==> (+)  -- insert operator
    \x y -> op y x ==> flip op
    \x -> x + y ==> (+ y)  -- insert section, 
    \x -> op x y ==> (`op` y)  -- insert section 
    \x -> y + x ==> (y +)  -- insert section
    \x -> \y -> ... ==> \x y -- lambda compression

<TEST>
f a = \x -> x + x -- f a x = x + x
f a = \a -> a + a -- f _ a = a + a
f a = \x -> x + x where _ = test
f = \x -> x + x -- f x = x + x
fun x y z = f x y z -- fun = f
fun x y z = f x x y z -- fun x = f x x
fun x y z = f g z -- fun x y = f g
fun mr = y mr
f = foo ((*) x) -- (x *)
f = (*) x
f = foo (flip op x) -- (`op` x)
f = flip op x
f = foo (flip (*) x) -- (* x)
f = foo (flip (-) x)
f = foo (\x y -> fun x y) -- fun
f = foo (\x y -> x + y) -- (+)
f = foo (\x -> x * y) -- (* y)
f = foo (\x -> x # y)
f = foo (\x -> \y -> x x y y) -- \x y -> x x y y
f = foo (\x -> \x -> foo x x) -- \_ x -> foo x x
f = foo (\(x:xs) -> \x -> foo x x) -- \(_:xs) x -> foo x x
f = foo (\x -> \y -> \z -> x x y y z z) -- \x y z -> x x y y z z
x ! y = fromJust $ lookup x y
f = foo (\i -> writeIdea (getClass i) i)
f = bar (flip Foo.bar x) -- (`Foo.bar` x)
f = a b (\x -> c x d)  -- (`c` d)
yes = \x -> a x where -- a
yes = \x y -> op y x where -- flip op
f = \y -> nub $ reverse y where -- nub . reverse
f = \z -> foo $ bar $ baz z where -- foo . bar . baz
f = \z -> foo $ bar x $ baz z where -- foo . bar x . baz
f = \z -> foo $ z $ baz z where
f = \x -> bar map (filter x) where -- bar map . filter
f = bar &+& \x -> f (g x)
foo = [\column -> set column [treeViewColumnTitle := printf "%s (match %d)" name (length candidnates)]]
foo = [\x -> x]
foo = [\m x -> insert x x m]
foo a b c = bar (flux ++ quux) c where flux = a -- foo a b = bar (flux ++ quux)
foo a b c = bar (flux ++ quux) c where flux = c
</TEST>
-}


module Hint.Lambda where

import Hint.Util
import Hint.Type
import Util
import Data.Maybe


lambdaHint :: DeclHint
lambdaHint _ _ x = concatMap (uncurry lambdaExp) (universeParentBi x) ++ concatMap lambdaDecl (universe x)


lambdaDecl :: Decl_ -> [Idea]
lambdaDecl (toFunBind -> o@(FunBind loc [Match _ name pats (UnGuardedRhs _ bod) bind]))
    | isNothing bind, isLambda $ fromParen bod = [err "Redundant lambda" o $ uncurry reform $ fromLambda $ Lambda an pats bod]
    | (pats2,bod2) <- etaReduce pats bod, length pats2 < length pats, pvars (drop (length pats2) pats) `disjoint` vars bind
        = [err "Eta reduce" (reform pats bod) (reform pats2 bod2)]
        where reform p b = FunBind loc [Match an name p (UnGuardedRhs an b) Nothing]
lambdaDecl _ = []


etaReduce :: [Pat_] -> Exp_ -> ([Pat_], Exp_)
etaReduce ps (App _ x (Var _ (UnQual _ (Ident _ y))))
    | ps /= [], PVar _ (Ident _ p) <- last ps, p == y, p /= "mr", y `notElem` vars x
    = etaReduce (init ps) x
etaReduce ps x = (ps,x)


lambdaExp :: Maybe Exp_ -> Exp_ -> [Idea]
lambdaExp p o@(Paren _ (App _ (Var _ (UnQual _ (Symbol _ x))) y)) | isAtom y, allowLeftSection x =
    [warn "Use section" o $ LeftSection an y (toNamed x)]
lambdaExp p o@(Paren _ (App _ (App _ (view -> Var_ "flip") (Var _ x)) y)) | allowRightSection $ fromNamed x =
    [warn "Use section" o $ RightSection an (QVarOp an x) y]
lambdaExp p o@Lambda{} | maybe True (not . isInfixApp) p, res <- niceLambda [] o, not $ isLambda res =
    [warn "Avoid lambda" o res]
lambdaExp p o@(Lambda _ _ x) | isLambda (fromParen x) && maybe True (not . isLambda) p =
    [warn "Collapse lambdas" o $ uncurry (Lambda an) $ fromLambda o]
lambdaExp _ _ = []


-- replace any repeated pattern variable with _
fromLambda :: Exp_ -> ([Pat_], Exp_)
fromLambda (Lambda _ ps1 (fromLambda . fromParen -> (ps2,x))) = (transformBi (f $ pvars ps2) ps1 ++ ps2, x)
    where f bad x@PVar{} | prettyPrint x `elem` bad = PWildCard an
          f bad x = x
fromLambda x = ([], x)
