{-# OPTIONS_GHC -fno-warn-unused-top-binds
                -fno-warn-type-defaults #-}

-- https://www.quora.com/What-are-20-lines-of-code-that-capture-the-essence-of-Haskell/answer/Tikhon-Jelvis?srid=zN7z

data DX a = DX { val :: a, dx :: DX a }

instance Show a => Show (DX a) where
  show (DX x (DX x' (DX x'' _))) = show [x, x', x'']

instance Num a => Num (DX a) where
  fromInteger n = DX (fromInteger n) 0
  DX x0 x' + DX y0 y' = DX (x0 + y0) (x' + y')
  DX x0 x' - DX y0 y' = DX (x0 - y0) (x' - y')
  x@(DX x0 x') * y@(DX y0 y') = DX (x0 * y0) (x*y' + y * x')
  signum (DX x0 _) = DX (signum x0) 0
  abs x@(DX x0 x') = DX (abs x0) (signum x * x')

instance Fractional n => Fractional (DX n) where
  fromRational n = DX (fromRational n) 0
  x@(DX x0 x') / y@(DX y0 y') =
    DX (x0 / y0) ((x' * y - x * y') / y ^ 2)

instance Eq a => Eq (DX a) where
  a == b = val a == val b

instance Ord a => Ord (DX a) where
  compare a b = compare (val a) (val b)

var :: Num a => a -> DX a
var x = DX x 1

data Op = Plus | Minus | Times | Divide deriving Eq
data Sym = Lit Double | Var String | Expr Op Sym Sym deriving Eq

instance Show Op where
  show op = case op of
    Plus -> " + "; Minus -> " - "; Times -> " * "; Divide -> " / "

instance Show Sym where
  show (Lit d) = show d
  show (Var v) = v
  show (Expr op sym1 sym2) = "(" ++ show sym1 ++ show op ++ show sym2 ++ ")"

instance Num Sym where
  fromInteger = Lit . fromInteger
  (+) = Expr Plus
  (-) = Expr Minus
  (*) = Expr Times

instance Fractional Sym where
  fromRational = Lit . fromRational
  (/) = Expr Divide

step :: Sym -> Sym
step sym =
  case sym of
    Expr op (Lit n1) (Lit n2) ->
      Lit $ (case op of Plus -> (+); Minus -> (-); Times -> (*); Divide -> (/)) n1 n2
    Expr op e1 e2 ->
      case (op, step e1, step e2) of
        (Plus, Lit 0.0, e) -> e
        (Plus, e, Lit 0.0) -> e
        (Minus, Lit 0.0, e) -> e
        (Minus, e, Lit 0.0) -> e
        (Times, Lit 1.0, e) -> e
        (Times, e, Lit 1.0) -> e
        (Times, Lit 0.0, _) -> Lit 0.0
        (Times, _, Lit 0.0) -> Lit 0.0
        (Divide, e, Lit 1.0) -> e
        (op', e1', e2') -> Expr op' e1' e2'
    atom -> atom

simplify :: Sym -> Sym
simplify = until . iterate step
  where until (x1 : x2 : xs)
          | x1 == x2 = x1
          | otherwise = until (x2 : xs)

main :: IO ()
main = do
  print $ (\x -> 7*x^2 + 3*x + 2) (var 5)
  -- [192,73,14]
  print $ simplify . val . dx $ (\x -> 7*x^2 + 3*x + 2) (var (Var "x"))
  -- ((7.0 * (x + x)) + 3.0)
