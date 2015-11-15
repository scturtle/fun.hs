{-# LANGUAGE GADTs #-}
import Control.Monad ((>=>))

data Free f a = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  -- fmap :: (a -> b) -> Free f a -> Free f b
  fmap k (Pure a) = Pure (k a)
  fmap k (Impure f) = Impure (fmap (fmap k) f)

instance Functor f => Applicative (Free f) where
  -- pure :: a -> Free f a
  pure = Pure
  -- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure f <*> k = fmap f k
  -- f :: f (Free f (a -> b))
  -- k :: Free f a
  Impure f <*> k = Impure (fmap (<*> k) f)

instance Functor f => Monad (Free f) where
  return = Pure
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure a >>= k = k a
  -- f :: f (Free f a)
  -- k :: a -> Free f b
  Impure f >>= k = Impure (fmap (>>= k) f)
  -- since f is self-recursion
  -- think this of diving into f with continutation k

-- Freer

data FFree f a where
    FPure :: a -> FFree f a
    -- env  &&  how to dive into it
    FImpure :: f x -> (x -> FFree f a) -> FFree f a

instance Functor (FFree f) where
  -- fmap :: (a -> b) -> FFree f a -> FFree f b
  fmap k (FPure a) = FPure (k a)
  --                                  (\x -> fmap k (k' x))
  fmap k (FImpure fx k') = FImpure fx (fmap k . k')

instance Applicative (FFree f) where
  pure = FPure
  -- (<*>) :: FFree f (a -> b) -> FFree f a -> FFree f b
  FPure a <*> k = fmap a k
  -- k' :: x -> FFree f (a -> b)
  FImpure fx k' <*> k = FImpure fx (\x -> k' x <*> k)

instance Monad (FFree f) where
  -- (>>=) :: FFree f a -> (a -> FFree f b) -> FFree f b
  FPure a >>= k = k a
  --                               (\x -> k' x >>= k)
  FImpure fx k' >>= k = FImpure fx (k' >=> k)

main :: IO ()
main = undefined
