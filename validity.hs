import qualified Data.Map as Map

{-
 - Propositional formulae validity checker in CPS style (port from
 - http://www.ps.uni-saarland.de/~duchier/python/continuations.html)
 -}

data Formula = Variable String
             | Negation Formula
             | Conjunction Formula Formula
             | Disjunction Formula Formula

instance Show Formula where
  show (Variable v) = v
  show (Negation f) = "!" ++ show f
  show (Conjunction p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
  show (Disjunction p q) = "(" ++ show p ++ " | " ++ show q ++ ")"

type Alist = Map.Map String Bool
type Yes = Alist -> No -> Bool
type No = Bool

isValid :: Formula -> Bool
isValid f = falsify f Map.empty (\_ _ -> False) True

satisfy :: Formula -> Alist -> Yes -> No -> Bool
satisfy (Conjunction p q) alist yes no =
  satisfy p alist
          (\alist' no' -> satisfy q alist' yes no')
          no

satisfy (Disjunction p q) alist yes no =
  satisfy p alist
          yes
          (satisfy q alist yes no)

satisfy (Negation p) alist yes no =
  falsify p alist yes no

satisfy (Variable v) alist yes no =
  case Map.lookup v alist of
    Just val -> if val then yes alist no else no
    Nothing  -> yes (Map.insert v True alist) no

falsify :: Formula -> Alist -> Yes -> No -> Bool
falsify (Conjunction p q) alist yes no =
  falsify p alist
          yes
          (falsify q alist yes no)

falsify (Disjunction p q) alist yes no =
  falsify p alist
          (\alist' no' -> falsify q alist' yes no')
          no

falsify (Negation p) alist yes no =
  satisfy p alist yes no

falsify (Variable v) alist yes no =
  case Map.lookup v alist of
    Just val -> if not val then yes alist no else no
    Nothing  -> yes (Map.insert v False alist) no

andF :: [Formula] -> Formula
andF = foldl1 Conjunction

orF :: [Formula] -> Formula
orF = foldl1 Disjunction

notF :: Formula -> Formula
notF = Negation

ifF :: Formula -> Formula -> Formula
ifF p = Disjunction (notF p)

main :: IO ()
main = do
  let p = Variable "p"
      q = Variable "q"
      r = Variable "r"
      f = ifF (andF [orF [p, q], ifF p r, ifF q r]) r
  print f
  print . isValid $ f
