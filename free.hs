{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE Rank2Types #-}

import Control.Monad (join,liftM)
import System.Exit (exitSuccess)

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  -- :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Monad (Free f) where
  return = Pure
  -- :: Free f a -> (a -> Free f b) -> Free f b
  x >>= f = joinF (fmap f x)

joinF :: Functor f => Free f (Free f a) -> Free f a
joinF (Pure a) = a
joinF (Free x) = Free (fmap joinF x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  -- :: Free f (a -> b) -> Free f a -> Free f b
  a <*> b = do x <- a; y <- b; return $ x y


{-
a forgetful functor U from the category of monads
to the category of functors:

  U(M,return,join) = M
-}

data Forget m a  = Forgotten {monad :: m a}

instance Monad m => Functor (Forget m) where
  fmap f (Forgotten x) = Forgotten (liftM f x)

{-
has left adjoint functor Free that lifts any functor to a free monad.

   Free F -> M
  =============
     F -> U M
-}

left :: (Functor f, Monad m) =>
            (forall a. f a -> m a) -> (forall a. Free f a -> m a)
left = foldF

right :: (Functor f, Monad m) =>
            (forall a. Free f a -> m a) -> (forall a. f a -> m a)
right g fa = g (liftF fa)


{-
The unit form of adjoint:

         liftF
  F(A) ---------> U(Free(A))
      `.           |
        `.         |
        g `.       | U(foldF g)
            `.     v
              `-> U(M(A))
-}

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

foldF :: (Functor f, Monad m) =>
           (forall a. f a -> m a) -> (forall a. Free f a -> m a)
foldF _ (Pure a) = return a
foldF g (Free x) = join (g (fmap (foldF g) x))


{- Application of lift: DSL
Better than fix-point of functor because we have compose for free. -}

data DSL k = Get (String -> k)
           | Put String k
           | End

instance Functor DSL where
  fmap f (Get k) = Get (f . k)
  fmap f (Put s k) = Put s (f k)
  fmap _ End = End

type MYIO = Free DSL

get :: MYIO String
get = liftF (Get id)

put :: String -> MYIO ()
put s = liftF (Put s ())

end :: MYIO a
end = liftF End

prog :: MYIO ()
prog = do s <- get; put s; end

runIO :: MYIO a -> IO a
runIO (Pure a) = return a
runIO (Free (Get k)) = getLine >>= runIO . k
runIO (Free (Put s k)) = putStrLn s >> runIO k
runIO (Free End) = exitSuccess

-- TODO: foldFree example

main :: IO ()
main = undefined
