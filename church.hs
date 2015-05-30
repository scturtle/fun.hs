{-# LANGUAGE RankNTypes #-}

{- https://karczmarczuk.users.greyc.fr/Essays/church.html
-- http://en.wikipedia.org/wiki/Church_encoding
-}

newtype Church = Ch (forall a. (a -> a) -> a -> a)

z :: Church
z = Ch $ \_ x -> x

s :: Church -> Church
s (Ch n) = Ch $ \f x -> f (n f x)

church :: Integer -> Church
church 0 = z
church n = s . church . pred $ n

nat :: Church -> Integer
nat (Ch n) = n succ 0

add :: Church -> Church -> Church
add (Ch m) (Ch n) = Ch $ \f -> m f . n f

mul :: Church -> Church -> Church
mul (Ch m) (Ch n) = Ch $ m . n

pow :: Church -> Church -> Church
pow (Ch m) (Ch n) = Ch $ n m

pre :: Church -> Church
pre (Ch n) = Ch $ \f x -> n (\g h -> h (g f)) (const x) id

sub :: Church -> Church -> Church
sub m (Ch n) = n pre m

main :: IO ()
main = do
  print $ nat $ s.s.s.s $ z
  print $ nat $ add (church 2) (church 2)
  print $ nat $ mul (church 2) (church 2)
  print $ nat $ pow (church 2) (church 2)
  print $ nat $ pre (church 5)
  print $ nat $ sub (church 5) (church 1)
