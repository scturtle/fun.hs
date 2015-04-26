import Data.Maybe (fromMaybe)

{-
 - my solutions of '20 Intermediate Haskell Exercises'
 - http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/
 -}

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  -- furry :: (a -> b) -> (x -> a) -> (x -> b)
  furry a2b x2a = a2b . x2a

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a)) = EitherLeft . Left $ f a
  furry _ (EitherLeft (Right b)) = EitherLeft . Right $ b

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left a)) = EitherRight . Left $ a
  furry f (EitherRight (Right b)) = EitherRight . Right $ f b

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  -- furry' f ma = banana (\a -> unicorn $ f a) ma
  -- furry' f = banana (\a -> unicorn $ f a)
  furry' f = banana $ unicorn . f

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f ma = concat $ furry f ma
  unicorn = (:[])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just a) = f a
  banana f Nothing = Nothing
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> m b) -> m a -> m b
  -- banana :: (a -> x -> b) -> (x -> a) -> (x -> b)
  -- banana ax2b x2a = \x -> let a = x2a x in ax2b a x
  -- banana ax2b x2a x = let a = x2a x in ax2b a x
  banana ax2b x2a x = ax2b (x2a x) x
  -- unicorn :: a -> m a
  -- unicorn :: a -> (x -> a)
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  -- banana :: (a -> m b) -> m a -> m b
  banana f (EitherLeft (Left a)) = f a
  banana _ (EitherLeft (Right b)) = EitherLeft $ Right b
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left a)) = EitherRight $ Left a
  banana f (EitherRight (Right b)) = f b
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
  -- banana :: (a -> m b) -> m a -> m b
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
-- apple ma mab = banana (\ab -> furry' ab ma) mab
apple = banana . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
  -- banana :: (a -> m b) -> m a -> m b
-- moppy as f = foldr (\a mbs -> banana (\b -> (banana (\bs -> unicorn (b : bs)) mbs)) (f a)) (unicorn []) as
moppy [] f = unicorn []
-- moppy (a:as) f = banana (\b -> banana (\bs -> unicorn (b:bs)) (moppy as f)) (f a)
moppy (a:as) f = apple (moppy as f) (furry' (:) (f a))

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage mas = moppy mas id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
-- apple :: (Misty m) => m a -> m (a -> b) -> m b
-- furry' :: (a -> b) -> m a -> m b
banana2 f2 ma mb = apple mb (furry' f2 ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
-- banana3 f3 ma mb mc = apple mc (apple mb (furry' f3 ma))
banana3 f3 ma mb mc = apple mc $ banana2 f3 ma mb

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f4 ma mb mc md = apple md $ banana3 f4 ma mb mc

newtype State s a = State {
  state :: s -> (s, a)
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  -- furry :: (a -> b) -> f a -> f b
  -- furry :: (a -> b) -> State s a -> State s b
  -- furry :: (a -> b) -> State { s -> (s, a) } -> State {s -> (s, b)}
  furry a2b s2sa = State $ \s -> let (s', a) = state s2sa s
                                     b = a2b a
                                 in  (s', b)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  -- banana :: (a -> m b) -> m a -> m b
  -- banana :: (a -> State s b) -> State s a -> State s b
  -- banana :: (a -> State {s -> (s, b)} -> State {s -> (s, a)} -> State {s -> (s, b)}
  banana a2sb sa = State $ \s -> let (s', a) = state sa s
                                     sb = a2sb a
                                 in  state sb s'

  -- unicorn :: a -> m a
  -- unicorn :: a -> State {s -> (s, a)}
  unicorn a = State $ \s -> (s, a)
