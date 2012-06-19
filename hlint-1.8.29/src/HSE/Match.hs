{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module HSE.Match where

import Data.Char
import HSE.Type
import HSE.Util
import qualified Language.Haskell.Exts as HSE_


class View a b where
    view :: a -> b


data App2 = NoApp2 | App2 Exp_ Exp_ Exp_ deriving Show

instance View Exp_ App2 where
    view (fromParen -> InfixApp _ lhs op rhs) = App2 (opExp op) lhs rhs
    view (fromParen -> App _ (fromParen -> App _ f x) y) = App2 f x y
    view _ = NoApp2


data App1 = NoApp1 | App1 Exp_ Exp_ deriving Show

instance View Exp_ App1 where
    view (fromParen -> App _ f x) = App1 f x
    view _ = NoApp1

data PVar_ = NoPVar_ | PVar_ String

instance View Pat_ PVar_ where
    view (fromPParen -> PVar _ x) = PVar_ $ fromNamed x
    view _ = NoPVar_

data Var_ = NoVar_ | Var_ String deriving Eq

instance View Exp_ Var_ where
    view (fromParen -> Var _ (UnQual _ x)) = Var_ $ fromNamed x
    view _ = NoVar_


(~=) :: Exp_ -> String -> Bool
(~=) = (==) . fromNamed


-- | fromNamed will return \"\" when it cannot be represented
--   toNamed may crash on \"\"
class Named a where
    toNamed :: String -> a
    fromNamed :: a -> String


isCon (x:_) = isUpper x || x == ':'
isCon _ = False

isSym (x:_) = not $ isAlpha x || x `elem` "_'"
isSym _ = False


instance Named (Exp S) where
    fromNamed (Var _ x) = fromNamed x
    fromNamed (Con _ x) = fromNamed x
    fromNamed (List _ []) = "[]"
    fromNamed _ = ""
    
    toNamed "[]" = List an []
    toNamed x | isCon x = Con an $ toNamed x
              | otherwise = Var an $ toNamed x

instance Named (QName S) where
    fromNamed (Special _ Cons{}) = ":"
    fromNamed (Special _ UnitCon{}) = "()"
    fromNamed (UnQual _ x) = fromNamed x
    fromNamed _ = ""

    toNamed ":" = Special an $ Cons an
    toNamed x = UnQual an $ toNamed x

instance Named HSE_.QName where
    fromNamed (HSE_.Special HSE_.Cons) = ":"
    fromNamed (HSE_.Special HSE_.UnitCon) = "()"
    fromNamed (HSE_.UnQual x) = fromNamed x
    fromNamed _ = ""

    toNamed ":" = HSE_.Special HSE_.Cons
    toNamed x = HSE_.UnQual $ toNamed x

instance Named (Name S) where
    fromNamed (Ident _ x) = x
    fromNamed (Symbol _ x) = x

    toNamed x | isSym x = Symbol an x
              | otherwise = Ident an x

instance Named HSE_.Name where
    fromNamed (HSE_.Ident x) = x
    fromNamed (HSE_.Symbol x) = x

    toNamed x | isSym x = HSE_.Symbol x
              | otherwise = HSE_.Ident x

instance Named (ModuleName S) where
    fromNamed (ModuleName _ x) = x
    toNamed = ModuleName an


instance Named (Pat S) where
    fromNamed (PVar _ x) = fromNamed x
    fromNamed (PApp _ x []) = fromNamed x
    fromNamed _ = ""

    toNamed x | isCon x = PApp an (toNamed x) []
              | otherwise = PVar an $ toNamed x


instance Named (TyVarBind S) where
    fromNamed (KindedVar _ x _) = fromNamed x
    fromNamed (UnkindedVar _ x) = fromNamed x
    toNamed x = UnkindedVar an (toNamed x)


instance Named (QOp S) where
    fromNamed (QVarOp _ x) = fromNamed x
    fromNamed (QConOp _ x) = fromNamed x
    toNamed x | isCon x = QConOp an $ toNamed x
              | otherwise = QVarOp an $ toNamed x

instance Named (Match S) where
    fromNamed (Match _ x _ _ _) = fromNamed x
    fromNamed (InfixMatch _ _ x _ _ _) = fromNamed x
    toNamed = error "No toNamed for Match"

instance Named (DeclHead S) where
    fromNamed (DHead _ x _) = fromNamed x
    fromNamed (DHInfix _ _ x _) = fromNamed x
    fromNamed (DHParen _ x) = fromNamed x
    toNamed = error "No toNamed for DeclHead"

instance Named (Decl S) where
    fromNamed (TypeDecl _ name _) = fromNamed name
    fromNamed (DataDecl _ _ _ name _ _) = fromNamed name
    fromNamed (GDataDecl _ _ _ name _ _ _) = fromNamed name
    fromNamed (TypeFamDecl _ name _) = fromNamed name
    fromNamed (DataFamDecl _ _ name _) = fromNamed name
    fromNamed (ClassDecl _ _ name _ _) = fromNamed name
    fromNamed (PatBind _ (PVar _ name) _ _ _) = fromNamed name
    fromNamed (FunBind _ (name:_)) = fromNamed name
    fromNamed (ForImp _ _ _ _ name _) = fromNamed name
    fromNamed (ForExp _ _ _ name _) = fromNamed name
    fromNamed _ = ""

    toNamed = error "No toNamed for Decl"
