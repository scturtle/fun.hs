import Text.Show.Functions

type Church a = (a -> a) -> a -> a

z :: Church a
z _ x = x

s :: Church a -> Church a
s n f x = f (n f x)

church :: Integer -> Church a
church 0 = z
church n = s .church $ n-1

unchurch :: Church Integer -> Integer
unchurch n = n (+ 1) 0

add :: Church a -> Church a -> Church a
add m n f = m f . n f

mul :: Church a -> Church a -> Church a
mul m n = m . n

pow :: Church a -> Church (a -> a) -> Church a
pow m n = n m

main :: IO ()
main = do
  print $ unchurch $ s.s.s.s $ z
  print $ unchurch $ add (church 2) (church 2)
  print $ unchurch $ mul (church 2) (church 2)
  print $ unchurch $ pow (church 2) (church 2)
