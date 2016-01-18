-- {-# LANGUAGE BangPatterns #-}

import Criterion.Main

class Queue q where
  it :: q a
  hd  :: q a -> a
  tl  :: q a -> q a
  snoc  :: q a -> a -> q a
  isEmpty :: q a -> Bool

data Q0 a = Q0 [a]

instance Queue Q0 where
  it = Q0 []
  hd (Q0 (x:_)) = x
  tl (Q0 (_:xs)) = Q0 xs
  snoc (Q0 xs) x = Q0 (xs ++ [x])
  isEmpty (Q0 l) = null l

data StrictList a = SNil | SCons a !(StrictList a)
data Q1 a = Q1 (StrictList a)

instance Queue Q1 where
  it = Q1 SNil
  hd (Q1 (SCons x _)) = x
  tl (Q1 (SCons _ xs)) = Q1 xs
  snoc (Q1 xs) x = Q1 (xs `app` SCons x SNil)
    where app SNil b = b
          app (SCons a as) b = SCons a (as `app` b)
  isEmpty (Q1 SNil) = True
  isEmpty _ = False

data Q2 a = Q2 !Int [a] !Int [a]

inv2 :: Q2 a -> Q2 a
inv2 q@(Q2 f xs r ys)
  | f < r = Q2 (f+r) (xs ++ reverse ys) 0 []
  | otherwise = q

instance Queue Q2 where
  it = Q2 0 [] 0 []
  hd (Q2 _ (x:_) _ _) = x
  tl (Q2 f (_:xs) r ys) = inv2 $ Q2 (f-1) xs r ys
  snoc (Q2 f xs r ys) z = inv2 $ Q2 f xs (r+1) (z:ys)
  isEmpty (Q2 f _ _ _) = f == 0

data Q3 a = Q3 !Int [a] Int !(StrictList a)

inv3 :: Q3 a -> Q3 a
inv3 q@(Q3 f xs r ys)
  | f < r = Q3 (f+r) (xs ++ rev ys) 0 SNil
  | otherwise = q
  where rev = go []
        go acc SNil = acc
        go acc (SCons x y) = go (x:acc) y

instance Queue Q3 where
  it = Q3 0 [] 0 SNil
  hd (Q3 _ (x:_) _ _) = x
  tl (Q3 f (_:xs) r ys) = inv3 $ Q3 (f-1) xs r ys
  snoc (Q3 f xs r ys) z = inv3 $ Q3 f xs (r+1) (SCons z ys)
  isEmpty (Q3 f _ _ _) = f == 0

-------------------------------------------------------------

test :: Queue q => q Int -> Int -> Bool
test q0 n = let q' = foldr (flip snoc) q0 [1..n]
            in (isEmpty (iterate tl q' !! n) || error "?")

main :: IO ()
main =
       let t0 = test (it :: Q0 Int)
           t1 = test (it :: Q1 Int)
           t2 = test (it :: Q2 Int)
           t3 = test (it :: Q3 Int)
       in  defaultMain [
               bgroup "100N" [ bench "Q0" $ whnf t0 100
                             , bench "Q1" $ whnf t1 100
                             , bench "Q2" $ whnf t2 100
                             , bench "Q3" $ whnf t3 100
                             ]
             , bgroup "1000N" [ bench "Q2" $ whnf t2 10000
                              , bench "Q3" $ whnf t3 10000
                              ]
             ]
