module Partial where

data Symbol : Set where
  X : Symbol
  Y : Symbol
  Z : Symbol

data Bool : Set where
  true : Bool
  false : Bool

_≡_ : Symbol → Symbol → Bool
X ≡ X = true
Y ≡ Y = true
Z ≡ Z = true
_ ≡ _ = false

data Expr : Set where
  Var : Symbol → Expr
  Lambda : Symbol → Expr → Expr
  App : Expr → Expr → Expr

data Function : Set where
  Def : Expr → Function

mutual
  data Env : Set where
    ε : Env
    _↦_,_ : Symbol → Value → Env → Env

  data Value : Set where
    Closure : Function → Env → Value
    Exception : Value

data Maybe (A : Set) : Set where
  Just : A → Maybe A
  Nothing : Maybe A

data Partial (A : Set) : Set where
  Now : A → Partial A
  Later : Partial A → Partial A

! : {A : Set} → Partial A → A
! (Now x) = x
! (Later x) = ! x

lookup : Symbol → Env → Maybe Value
lookup sym ε = Nothing
lookup sym (sym′ ↦ val , env) with sym ≡ sym′
... | true = Just val
... | false = lookup sym env

mutual

  interp : Expr → Env → Partial Value
  interp (Var x) env with lookup x env
  ... | Just val = Now val
  ... | Nothing = Now Exception
  interp (Lambda x e) env = Now (Closure (Def (Lambda x e)) env)
  interp (App e₁ e₂) env = Later (app (interp e₁ env) (interp e₂ env))

  app : Partial Value → Partial Value → Partial Value
  app (Later f) a = Later (app f a)
  app (Now (Closure (Def (Lambda x e)) env)) a = interp e (x ↦ ! a , env)
  app (Now Exception) _ = Now Exception
  app _ _ = Now Exception

-- TODO: Termination checking failed for the following functions: interp, app.
