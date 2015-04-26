import Data.List (findIndex)
import Prelude hiding (exp)
import Data.IORef
import System.IO.Unsafe

{-
 - CPS transformer (EOPL ch. 6)
 -}

{-# NOINLINE varCnt #-}
varCnt :: IORef Int
varCnt = unsafePerformIO (newIORef 0)

{-# NOINLINE freshVar #-}
freshVar :: () -> String
freshVar() =
  unsafePerformIO $ do
    n <- readIORef varCnt
    writeIORef varCnt (n+1)
    return("v" ++ show n)

data InpExp = Const Int
            | Var String
            | Sub InpExp InpExp
            | Eq0 InpExp
            | If InpExp InpExp InpExp
            | Let String InpExp InpExp
            | Letrec [String] [[String]] [InpExp] InpExp
            | Proc [String] InpExp
            | Call InpExp [InpExp]
              deriving Show

data SimpleExp = Const' Int
               | Var' String
               | Sub' SimpleExp SimpleExp
               | Eq0' SimpleExp
               | Proc' [String] TfExp
                 deriving Eq

instance Show SimpleExp where
  show e =
    case e of
      Const' n -> show n
      Var' s -> s
      Sub' a b -> "(-" ++ show a ++ " " ++ show b ++ ")"
      Eq0' a -> "(zero? " ++ show a ++ ")"
      Proc' ids body -> "(lambda (" ++ unwords ids ++ ") " ++ show body ++ ")"

data TfExp = SimExp SimpleExp
           | If' SimpleExp TfExp TfExp
           | Let' String SimpleExp TfExp
           | Letrec' [String] [[String]] [TfExp] TfExp
           | Call' SimpleExp [SimpleExp]
             deriving Eq

instance Show TfExp where
  show e =
    case e of
      SimExp e' -> show e'
      If' e1 e2 e3 -> "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
      Let' _id e1 e2 -> "(let ([" ++ _id ++ " " ++ show e1 ++ "]) " ++ show e2 ++ ")"
      Letrec' names varss bodies recbody ->
        "(letrec (" ++ unwords ["[" ++ nm ++ " (lambda (" ++ unwords vs ++ ") " ++ show bd ++ ")]"
                             | (nm, vs, bd) <- zip3 names varss bodies]
        ++ ") " ++ show recbody ++ ")"
      Call' rator rands -> "(" ++ show rator ++ concat [" " ++ show d | d <- rands] ++ ")"

isSimple :: InpExp -> Bool
isSimple exp =
  case exp of
    (Const _) -> True
    (Var _) -> True
    (Sub a b) -> isSimple a && isSimple b
    (Eq0 a) -> isSimple a
    (Proc _ _) -> True
    _ -> False

cpsSimple :: InpExp -> SimpleExp
cpsSimple exp =
  case exp of
    (Const n) -> Const' n
    (Var s) -> Var' s
    (Sub a b) -> Sub' (cpsSimple a) (cpsSimple b)
    (Eq0 a) -> Eq0' (cpsSimple a)
    (Proc ids body) -> Proc' (ids ++ ["k"]) (cpsExp body (Var' "k"))
    e -> error $ "Naive: " ++ show e

cpsExps :: [InpExp] -> ([SimpleExp] -> TfExp) -> TfExp
cpsExps es builder = cpsRest es
  where cpsRest :: [InpExp] -> TfExp
        cpsRest exps =
          case findIndex (not . isSimple) exps of
            Nothing -> builder $ map cpsSimple exps
            Just pos -> let var = freshVar()
                        in  cpsExp (exps !! pos)
                                   (Proc' [var] (cpsRest (take (pos - 1) exps
                                                          ++ [Var var] ++
                                                          drop (pos + 1) exps)))

cpsExpCtx :: InpExp -> (SimpleExp -> TfExp) -> TfExp
cpsExpCtx exp ctx =
  if isSimple exp then ctx (cpsSimple exp)
                  else let var = freshVar()
                       in  cpsExp exp (Proc' [var] (ctx (Var' var)))

cpsExp :: InpExp -> SimpleExp -> TfExp
cpsExp exp k =
  case exp of
    Const n -> send2cont $ Const' n
    Var n -> send2cont $ Var' n
    Proc vars body -> send2cont $ Proc' (vars ++ ["k"]) (cpsExp body (Var' "k"))
    Sub e1 e2 -> cpsExps [e1, e2] $ \[e1', e2'] -> send2cont $ Sub' e1' e2'
    Eq0 e1 -> cpsExpCtx e1 $ \e1' -> send2cont $ Eq0' e1'
    -- If e1 e2 e3 -> cpsExpCtx e1 $ \e1' -> If' e1' (cpsExp e2 k) (cpsExp e3 k)
    If e1 e2 e3 -> cpsExpCtx e1 $ \e1' ->
      if isIdFunc k || k == Var' "k"
         then If' e1' (cpsExp e2 k) (cpsExp e3 k)
         else Let' "k" k (If' e1' (cpsExp e2 (Var' "k")) (cpsExp e3 (Var' "k")))
    -- Let _id rhs body ->  cpsExpCtx rhs $ \rhs' -> Let' _id rhs' (cpsExp body k)
    Let _id rhs body -> cpsExp rhs (Proc' [_id] (cpsExp body k))
    Call rator rands -> cpsExpCtx rator $ \rator' ->
                        cpsExps rands $ \rands' -> Call' rator' (rands' ++ [k])
    Letrec names varss bodies recbody ->
      Letrec' names [vars ++ ["k"] | vars <- varss]
                    [cpsExp body (Var' "k") | body <- bodies]
                    (cpsExp recbody k)
  where isIdFunc (Proc' [v1] (SimExp (Var' v2))) = v1 == v2
        isIdFunc _ = False
        send2cont e = if isIdFunc k then SimExp e else Call' k [e]

cps :: InpExp -> TfExp
cps exp = cpsExpCtx exp SimExp

main :: IO ()
main = do
  let test e = do writeIORef varCnt 0; print . cps $ e
  test $ Var "x"
  test $ Proc ["x"] (Call (Var "x") [Const 1])
  test $ If (Call (Var "f") [Var "x"]) (Var "a") (Var "b")
  test $ If (Var "x") (Call (Var "f") [Var "a"]) (Var "b")
  test $ Proc ["x"] $ If (Call (Var "f") [Var "x"]) (Var "a") (Var "b")
  test $ Proc ["x"] $ If (If (Var "x") (Call (Var "f") [Var "a"]) (Var "b")) (Var "c") (Var "d")
