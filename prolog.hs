{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
import Data.String (IsString(fromString))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

infix 6 :-
data Term = Var String Int | Cons String [Term] deriving Eq
data Rule  = Term :- Term
type Subs = [(Term, Term)]
type Rules = [Rule]

instance Show Term where
  show (Var s n) = if n == 0 then s else s ++ show n
  show (Cons s ts) = s ++ "(" ++ intercalate "," (fmap show ts) ++ ")"

instance Show Rule where
  show (t1 :- t2) = show t1 ++ " :- " ++ show t2

instance IsString Term where fromString = parseTerm
instance IsString Rule where fromString = parseRule

term :: Parser Term
term = do
  name <- many1 alphaNum
  (do ts <- char '(' *> term `sepBy` (char ',') <* char ')'
      return $ Cons name ts)
    <|> return (Var name 0)

clause :: Parser Rule
clause = do
  t1 <- term
  (do string " :- "
      t2 <- term
      return $ t1 :- t2)
    <|> return (t1 :- Cons "and" [])

parseTerm :: String -> Term
parseTerm s =
  case parse (term <* eof) "" s of
    Left e -> error $ show e
    Right t -> t

parseRule :: String -> Rule
parseRule s =
  case parse (clause <* eof) "" s of
    Left e -> error $ show e
    Right c -> c

deref :: Subs -> Term -> Term
deref env t = fromMaybe t (lookup t env)

bind :: Subs -> Term -> Term -> Subs
bind env v t = (v, t) : env

isVar :: Term -> Bool
isVar (Var _ _) = True
isVar _ = False

unify :: Subs -> Term -> Term -> _ -> _ -> _
unify env t1 t2 yes no
    | t1' == t2' = yes env no
    | isVar t1' = yes (bind env t1' t2') no
    | isVar t2' = yes (bind env t2' t1') no
    | otherwise =
        let Cons fun1 args1 = t1'
            Cons fun2 args2 = t2'
        in  if fun1 == fun2 && length args1 == length args2
              then unifyList env args1 args2 yes no
              else no
  where t1' = deref env t1
        t2' = deref env t2

unifyList :: Subs -> [Term] -> [Term] -> _ -> _ -> _
unifyList env [] [] yes no = yes env no
unifyList env (t1:l1') (t2:l2') yes no = unify env t1 t2 yes' no
  where yes' env' no' = unifyList env' l1' l2' yes no'

solve :: Subs -> Rules -> Term -> _ -> _ -> _
solve env rules t@(Var _ _) yes no entryno =
  let t' = deref env t
  in  if t' == t then error "cannot call on uninstantiated literal"
                 else solve env rules t' yes no entryno
solve env rules t@(Cons fun _) yes no entryno =
  call env rules fun t yes no entryno

solveAll :: Subs -> Rules -> [Term] -> _ -> _ -> _
solveAll env rules [] yes no entryno = yes env no
solveAll env rules [x] yes no entryno = solve env rules x yes no entryno
solveAll env rules (x:l) yes no entryno = solve env rules x yes' no entryno
  where yes' env' no' = solveAll env' rules l yes no' entryno

solveSome :: Subs -> Rules -> [Term] -> _ -> _ -> _
solveSome env rules [] yes no entryno = no
solveSome env rules [x] yes no entryno = solve env rules x yes no entryno
solveSome env rules (x:l) yes no entryno = solve env rules x yes no' entryno
  where no' = solveSome env rules l yes no entryno

execute :: Subs -> Rules -> Rules -> Int -> Term -> _ -> _ -> _ -> _
execute env rules [x] i literal yes no entryno =
    tryrule env rules x i literal yes no entryno
execute env rules (x:l) i literal yes no entryno =
    tryrule env rules x i literal yes no' entryno
  where no' = execute env rules l (i+1) literal yes no entryno

rename :: Int -> Rule -> Rule
rename i (hd :- body) = rename' hd :- rename' body
  where rename' (Var s _) = Var s i
        rename' (Cons s ts) = Cons s (map rename' ts)

tryrule :: Subs -> Rules -> Rule -> Int -> Term -> _ -> _ -> _ -> _
tryrule env rules rule i literal yes no entryno = unify env hd literal yes' no
  where hd :- body = rename i rule
        yes' env' no' = solve env' rules body yes no' entryno

call :: Subs -> Rules -> String -> Term -> _ -> _ -> _ -> _
call env rules predicate literal@(Cons _ args) yes no entryno =
  case predicate of
    "and" -> solveAll env rules args yes no entryno
    "or" -> solveSome env rules args yes no entryno
    "cut" -> yes env entryno
    _ -> case [c | c@((Cons fun _) :- _) <- rules, fun == predicate] of
           [] -> error $ "unknown predicate: " ++ predicate
           rs -> execute env rs rs 1 literal yes no no

query :: Rules -> Term -> Maybe _
query rules q = solve [] rules q defaultYes defaultNo defaultNo
  where defaultYes env no = Just env
        defaultNo = Nothing

main :: IO ()
main = do
  let rules = ["member(x,cons(x,l))"
              ,"member(x,cons(y,l)) :- member(x,l)"]
  print $ query rules "member(x,l)"
  -- Just [(l,cons(x1,l1)),(x1,x)]
  print $ query rules "member(x,nil())"
  -- Nothing
