module Partiality where

{- unsuccessful port from http://www.soimort.org/posts/programs-and-proofs/ -}

open import Data.Bool using (Bool; false; true)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Char using (_==_) renaming (Char to Symbol)
open import Coinduction using (∞; ♯_; ♭)
open import Category.Monad.Partiality renaming (monad to PM)
open import Category.Monad using (RawMonad; module RawMonad)

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

-- data Partial (A : Set) : Set where
--   now : A → Partial A
--   later : ∞ (Partial A) → Partial A

-- infixl 40 _>>=_
-- _>>=_ : {a b : Set} -> Partial a -> (a -> Partial b) -> Partial b
-- (now x) >>= f = f x
-- (later x) >>= f = later (♯ (♭ x >>= f))

mutual

  interp : Expr → Env → Value ⊥
  interp (var x) env with lookup x env
  ... | just val = now val
  ... | nothing = now exception
  interp (lambda x e) env = now (closure (def (lambda x e)) env)
  interp (app e₁ e₂) env = later (♯ apply (interp e₁ env) (interp e₂ env))

  apply : Value ⊥ → Value ⊥ → Value ⊥
  apply (later f) a = later (♯ apply (♭ f) a)
  apply (now (closure (def (lambda x e)) env)) a =
    let open RawMonad PM in a >>= \a′ → interp e (x ↦ a′ , env)
  apply _ _ = now exception

-- Error: Termination checking failed for the following functions: interp, apply.
