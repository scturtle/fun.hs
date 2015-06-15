{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE Rank2Types #-}
{- https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation -}

main :: IO ()
main = undefined

data LensR s a = L { viewR :: s -> a
                   , setR :: a -> s -> s }

composeL :: LensR a b -> LensR b c -> LensR a c
composeL (L v1 u1) (L v2 u2) =
  L (v2 . v1) (\a s -> u1 (u2 a (v1 s)) s)

overR :: LensR s a -> (a -> a) -> s -> s
overR l f s = setR l (f (viewR l s)) s



-- what about more?

-- modifyM :: LensR s a -> (a -> Maybe a) -> s -> Maybe s
-- modifyIO :: LensR s a -> (a -> IO a) -> s -> IO s

-- data LensR s a = L { viewR :: s -> a
--                    , setR :: a -> s -> s
--                    , mod :: (a -> a) -> s -> s
--                    , modM ::  (a -> Maybe a) -> s -> Maybe s
--                    , modIO :: (a -> IO a) -> s -> IO s }

-- {- perhaps... -}     modF :: Functor f => (a -> f a) -> s -> f s



-- Edward's insight: just one function!
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

set' :: Lens' s a -> a -> s -> s
-- set' l a s = runIdentity $ l (\_ -> Identity a) s
set' l a = runIdentity . l (Identity . const a)

newtype Const v a = Const { getConst :: v }

instance Functor (Const v) where
  fmap _ (Const x) = Const x

view' :: Lens' s a -> s -> a
-- view' l s = getConst $ l Const {- :: a -> Const a a -} s
view' l = getConst . l Const



-- how to make a lens
data Person = P { _name :: String, _salary :: Int }

name :: Lens' Person String
-- name :: Functor f => (String -> f String)
                  -- -> (Person -> f Person)
name fn (P n s) = updName <$> fn n
  where updName :: String -> Person
        updName n' = P n' s



-- lens composition is simply function composition (.)
-- (.) :: (b -> c)    -> (a -> b)   -> (a -> c)
-- (.) :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
-- (.) :: ((s2 -> f s2) -> (s1 -> f s1)) ->
--        ((a  -> f  a) -> (s2 -> f s2)) ->
--        ((a  -> f  a) -> (s1 -> f s1))



-- Edward's second insight

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- multi-focus lenses
data Address = A { _road :: String
                 , _city :: String
                 , _postcode :: String }

type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

road :: Lens' Address String
road fn (A r c p) = (\r' -> A r' c p) <$> fn r

-- focus on road and city
addrStrs :: Traversal' Address String
addrStrs fn (A r c p) = (\r' c' -> A r' c' p) <$> fn r <*> fn c



-- the REAL story

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens' s a = Lens s s a a

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> b -> s -> t
set l a = runIdentity . l (Identity . const a)

view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- & is flip $
-- %~ is over
-- .~ is set
-- ^. is view
-- for nat: +~, -~, *~, //~, ^~, ^^~, **~
-- for logic: ||~, &&~
-- for tuple: _1, _2, both
-- for state: .=, use
-- the mappend for monoid: <>~
-- result is either: ^?



-- the category view: store comonad

class Functor w => Comonad w where
  -- dual to return
  extract :: w a -> a

  -- dual to join
  duplicate :: w a -> w (w a)
  duplicate = extend id

  -- dual to bind
  extend :: (w a -> b) -> w a -> w b
  extend f x = fmap f (duplicate x)

-- fmap from comonad
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)


-- one center pos and a function peek the value at any pos
data Store a b = Store { pos :: a, peek :: a -> b }

instance Functor (Store a) where
  -- apply f at value of any pos
  -- :: (b -> c) -> Store a b -> Store a c
  fmap f (Store a a2b) = Store a (f . a2b)

instance Comonad (Store a) where
  -- return the value at the center
  -- :: Store a b -> b
  extract (Store a a2b) = a2b a

  -- the value of any pos is now a copy of
  -- this store that centered at this pos
  -- :: Store a b -> Store a (Store a b)
  duplicate (Store a a2b) = Store a (`Store` a2b)

  -- :: (Store a b -> c) -> Store a b -> Store a c
  extend f (Store a a2b) = Store a (f . (`Store` a2b))


type Lens'' a b = a -> Store b a

road' :: Lens'' Address String
road' a@(A r _ _) = Store r (\r' -> a {_road = r'})

-- one way
view'' :: Lens'' a b -> a -> b
view'' l a = pos $ l a

set'' :: Lens'' a b -> a -> b -> a
set'' l a = peek $ l a

-- another way
lens :: (a -> b) -> (a -> b -> a) -> Lens'' a b
lens v s a = Store (v a) (s a)
