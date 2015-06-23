{-# LANGUAGE TemplateHaskell #-}
module Printf (printf) where

import Language.Haskell.TH
import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)

-- modified from "24 Days of GHC Extensions: Template Haskell"
-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String deriving Show

-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize [] = []
tokenize ('%':c:rest) | c == 'd' = D : tokenize rest
                      | c == 's' = S : tokenize rest
tokenize (s:str) = L (s:p) : tokenize rest -- don't get stuck on weird '%'
    where (p,rest) = span (/= '%') str

-- generate argument list for the function
args :: [(Format, Name)] -> [PatQ]
args = mapMaybe gen
  where gen (f, n) =
          case f of L _ -> Nothing
                    _   -> Just $ varP n

-- generate body of the function
body :: [(Format, Name)] -> ExpQ
body = foldr1 (\e e' -> [|$e ++ $e'|]) . map gen
  where gen (f, n) =
          case f of L s -> stringE s
                    D   -> [|show $(varE n)|]
                    S   -> varE n

-- glue the argument list and body together into a lambda
printf :: String -> Q Exp
printf format = do
  let fmt = tokenize format
  names <- replicateM (length fmt) (newName "x")
  let fmt' = zip fmt names
  lamE (args fmt') (body fmt')
