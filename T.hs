{- Gödel's System T -}

data Exp = Var String
         | Z | S Exp
         | Rec Exp Exp String String Exp
         | Lam {- Typ -} String Exp
         | App Exp Exp
  deriving (Eq, Show)

isClosed :: Exp -> Bool
isClosed Z = True
isClosed (S e) = isClosed e
isClosed Lam{} = True
isClosed _ = False

subst :: String -> Exp -> Exp -> Exp
subst a b expr =
  case expr of
    Var x -> if a == x then b else Var x
    S e -> S $ subst a b e
    Rec e e0 x y e1 ->
      let e1' = if x == a || y == a then e1 else subst a b e1
      in  Rec (subst a b e) (subst a b e0) x y e1'
    Lam x e -> if a == x then Lam x e else Lam x (subst a b e)
    App e1 e2 -> App (subst a b e1) (subst a b e2)
    e -> e

eval :: Exp -> Exp
eval e | isClosed e = e
eval (S e) = S (eval e)
eval (App e1 e2) =
  let Lam x e = eval e1
  in  eval (subst x (eval e2) e)
eval (Rec e e0 x y e1) =
  case eval e of
    Z    -> eval e0
    S e' -> let y' = eval (Rec e' e0 x y e1)
            in  eval (subst x e' (subst y y' e1))

main :: IO ()
main = do
  let add = Lam "a" $ Lam "b" $ Rec (Var "a") (Var "b") "_" "y" (S (Var "y"))
      two = S (S Z)
  print . eval $ App (App add two) two
  -- S (S (S (S Z)))
