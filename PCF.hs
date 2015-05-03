{-# LANGUAGE OverloadedStrings #-}
import Data.String (IsString(fromString))

data Typ = TNat | TArr Typ Typ
  deriving (Show, Eq)

data Exp = Var String
         | Nat Int | Suc Exp | Prd Exp
         | Mul Exp Exp
         | Ifz Exp Exp Exp
         | App Exp Exp
         | Lam Typ String Exp
         | Fix Typ String Exp
  deriving (Show, Eq)

instance IsString Exp where fromString = Var

isVal :: Exp -> Bool
isVal Nat{} = True
isVal Lam{} = True
isVal _ = False

subst :: String -> Exp -> Exp -> Exp
subst a b expr =
  case expr of
    Var x -> if x == a then b else expr
    Suc e -> Suc $ subst a b e
    Prd e -> Prd $ subst a b e
    Mul e1 e2 -> Mul (subst a b e1) (subst a b e2)
    Ifz e e0 e1 -> Ifz (subst a b e) (subst a b e0) (subst a b e1)
    App e1 e2 -> App (subst a b e1) (subst a b e2)
    Lam t x e -> Lam t x (if x == a then e else subst a b e)
    Fix t x e -> Fix t x (if x == a then e else subst a b e)
    e -> e

eval :: Exp -> Exp
eval e | isVal e = e
eval (Suc e) = case eval e of Nat n -> Nat (n + 1)
eval (Prd e) = case eval e of Nat n -> Nat (n - 1)
eval (Mul e1 e2) = case (eval e1, eval e2) of (Nat a, Nat b) -> Nat $ a * b
eval (Ifz e e0 e1) = eval $ if eval e == Nat 0 then e0 else e1
eval (App e1 e2) =
  let Lam _ x e = eval e1
      e2' = eval e2
  in eval $ subst x e2' e
eval fix@(Fix _ x e) = eval $ subst x fix e

main :: IO ()
main = do
  let fac = Fix (TArr TNat TNat) "fac" $
              Lam TNat "x" (Ifz "x" (Nat 1) (Mul "x" (App "fac" (Prd "x"))))
  print . eval $ App fac (Nat 4)
