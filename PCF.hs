{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromMaybe, fromJust)
import Data.String (IsString(fromString))
import Control.Monad.State

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

subst :: String -> Exp -> Exp -> Exp
subst a b expr =
  case expr of
    Var x -> if x == a then b else expr
    Suc e -> Suc $ subst a b e
    Prd e -> Prd $ subst a b e
    Mul e1 e2 -> Mul (subst a b e1) (subst a b e2)
    Ifz e e1 e2 -> Ifz (subst a b e) (subst a b e1) (subst a b e2)
    App e1 e2 -> App (subst a b e1) (subst a b e2)
    Lam t x e -> Lam t x (if x == a then e else subst a b e)
    Fix t x e -> Fix t x (if x == a then e else subst a b e)
    e -> e

eval :: Exp -> Exp
eval expr =
  case expr of
    Nat{} -> expr
    Lam{} -> expr
    Suc e -> let Nat n = eval e in Nat (n + 1)
    Prd e -> let Nat n = eval e in Nat (n - 1)
    Mul e1 e2 -> let (Nat a, Nat b) = (eval e1, eval e2) in Nat $ a * b
    Ifz e e1 e2 -> eval $ if eval e == Nat 0 then e1 else e2
    App e1 e2 -> let Lam _ x e = eval e1 in eval $ subst x (eval e2) e
    f@(Fix _ x e) -> eval $ subst x f e

typeOf :: [(String, Typ)] -> Exp -> Typ
typeOf env expr =
  case expr of
    Var x -> fromMaybe (error "unbounded") (lookup x env)
    Nat _ -> TNat
    Suc e -> case typeOf env e of TNat -> TNat; _ -> error "unmatch"
    Prd e -> case typeOf env e of TNat -> TNat; _ -> error "unmatch"
    Mul e1 e2 ->
      case (typeOf env e1, typeOf env e2) of
        (TNat, TNat) -> TNat
        _            -> error "unmatch"
    Ifz e e1 e2 ->
      case (typeOf env e, typeOf env e1, typeOf env e2) of
        (TNat, t1, t2) -> if t1 == t2 then t1 else error "unmatch"
        _ -> error "unmatch"
    App e1 e2 ->
      case (typeOf env e1, typeOf env e2) of
        (TArr t11 t12, t2) -> if t11 == t2 then t12 else error "unmatch"
        _ -> error "unmatch"
    Lam t@(TArr i o) x e ->
      let o' = typeOf ((x, i) : env) e
      in  if o == o' then t else error "unmatch"
    Fix t x e ->
      let t' = typeOf ((x, t) : env) e
      in  if t' == t then t else error "unmatch"

eval' :: Exp -> State [(String, Exp)] Exp
eval' expr =
  case expr of
    Nat {} -> return expr
    Lam {} -> return expr
    Var x  -> fromJust <$> gets (lookup x)
    Suc e     -> (\(Nat n) -> Nat (n + 1))       <$> eval' e
    Prd e     -> (\(Nat n) -> Nat (n - 1))       <$> eval' e
    Mul e1 e2 -> (\(Nat a) (Nat b) -> Nat (a*b)) <$> eval' e1 <*> eval' e2
    Ifz e e1 e2   -> do v <- eval' e; eval' (if v == Nat 0 then e1 else e2)
    Fix _ x e -> modify ((x, e) :) >> eval' e
    App e1 e2 ->
      do Lam _ x e <- eval' e1
         e2' <- eval' e2
         modify ((x, e2') :)
         eval' e

main :: IO ()
main = do
  let fac = Fix (TArr TNat TNat) "fac" $
              Lam (TArr TNat TNat) "x" (Ifz "x" (Nat 1) (Mul "x" (App "fac" (Prd "x"))))
      prog = App fac (Nat 4)
  -- print . typeOf [] $ prog
  print . eval $ prog
  -- print $ evalState (eval' fac) []
  print $ evalState (eval' prog) []
