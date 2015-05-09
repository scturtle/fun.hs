module Partiality where

{- port from http://www.soimort.org/posts/programs-and-proofs/ -}

open import Data.Bool using (Bool; false; true)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Char using (_==_) renaming (Char to Symbol)
open import Coinduction using (∞; ♯_; ♭)
-- open import Category.Monad.Partiality renaming (monad to PM)
-- open import Category.Monad using (RawMonad; module RawMonad)

data Expr : Set where
  var : Symbol → Expr
  lambda : Symbol → Expr → Expr
  app : Expr → Expr → Expr

data Function : Set where
  def : Expr → Function

mutual

  data Env : Set where
    ε : Env
    _↦_,_ : Symbol → Value → Env → Env

  data Value : Set where
    closure : Function → Env → Value
    exception : Value

lookup : Symbol → Env → Maybe Value
lookup sym ε = nothing
lookup sym (sym′ ↦ val , env) with sym == sym′
... | true = just val
... | false = lookup sym env

-- partiality monad
data _⊥ (A : Set) : Set where
  now : A → A ⊥
  later : ∞ (A ⊥) → A ⊥

return : {a : Set} → a → a ⊥
return = now

_>>=_ : {a b : Set} → a ⊥ → (a → b ⊥) → b ⊥
(now x) >>= f = f x
(later x) >>= f = later (♯ (♭ x >>= f))
infixl 40 _>>=_

{-# TERMINATING #-}
force : Value ⊥ → Value
force (now x) = x
force (later x) = force (♭ x)

mutual

  {-# TERMINATING #-} -- TODO: ???
  interp : Expr → Env → Value ⊥
  interp (var x) env with lookup x env
  ... | just val = now val
  ... | nothing = now exception
  interp (lambda x e) env = now (closure (def (lambda x e)) env)
  interp (app e₁ e₂) env = later (♯ apply (interp e₁ env) (interp e₂ env))

  apply : Value ⊥ → Value ⊥ → Value ⊥
  apply (later f) a = later (♯ apply (♭ f) a)
  apply (now (closure (def (lambda x e)) env)) a =
                               a >>= \a′ → interp e (x ↦ a′ , env)
    -- let open RawMonad PM in a >>= \a′ → interp e (x ↦ a′ , env)
  apply _ _ = now exception

e0 = ε
t1 = lambda 'a' (var 'a')
t2 = app (lambda 'x' (var 'x')) (lambda 'a' (var 'a'))
t3 = app (app (lambda 'f' (lambda 'x' (app (var 'f') (var 'x'))))
              (lambda 'a' (var 'a')))
         (lambda 'b' (var 'b'))
t4 = app (lambda 'x' (app (var 'x') (var 'x')))
         (lambda 'x' (app (var 'x') (var 'x')))

r1 = force (interp t1 e0)
r2 = force (interp t2 e0)
r3 = force (interp t3 e0)
r4 = force (interp t4 e0)
