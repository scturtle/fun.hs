(A note of https://www.fpcomplete.com/user/bartosz/understanding-algebras .)

For a categroy K and endofunctor `F : K -> K`,
(here K is Hask -- the category of all Haskell types),
an F-algebra is a pair (A, a) where
A is object in K and a : F(A) -> A is arrow in K.

Defined in Control.Functor.Algebra:

> type Algebra f a = f a -> a

Fix point of a type constructor f:

> newtype Fix f = Fix { unFix :: f (Fix f) }

Given functor F, `Fix F` is a new unique type.
    Fix : f (Fix f) -> Fix f
and
    unFix : Fix f -> f (Fix f)
prove that `Fix F` is the initial algebra in F-Alg.
Also called the fixed point of the functor F.


           fmap h
F (Fix F) ----------> F(A)
     |^                |
     ||                |
 Fix || unFix          | a
     ||                |
     v|                v
   Fix F ----------->  A
              h


This means there exists a (unique) homomophism
`h :: Fix f -> a` for any f-algebra `f a -> a`:

    h = a . fmap h . unFix

The builder function is called a catamorphism:

> cata :: Functor f => (f a -> a) -> (Fix f -> a)
> cata a = a . fmap (cata a) . unFix

It takes an arbitray f-algebra and produce
the F-homomorphism from initial algebra to it.

Also can be viewed as from a non-recursive function `f a -> a`
to recursive evaluator for a nested data structure.

E.g. `ListF a b` where a is the type of element and
b is the type we recurse into (the result).

> data ListF a b = Nil | Cons a b
>
> instance Functor (ListF a) where
>   -- :: (b -> c) -> ListF a b -> ListF a c
>   fmap _ Nil = Nil
>   fmap f (Cons e x) = Cons e (f x)

The type `List a` is just the fixed point of fucntor `ListF a`:

> type List a = Fix (ListF a)
>
> lst :: List Int
> lst = Fix $ Cons 1 (Fix $ Cons 2 (Fix $ Cons 3 (Fix Nil)))

Traversing and evaluating on recursive data structure (e.g. `foldr` here)
can be build from simple f-algebra via catamorphism:

> sum' :: ListF Int Int -> Int
> sum' Nil = 0
> sum' (Cons e acc) = e + acc
>
> toStr :: ListF Int String -> String
> toStr Nil = "[]"
> toStr (Cons e acc) = show e ++ ":" ++ acc
>
> main :: IO ()
> main = do
>   print $ cata sum' lst
>   print $ cata toStr lst
