{-# LANGUAGE DeriveDataTypeable #-}
module LambdaQ (Expr(..), lambda) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.Data (Data, Typeable)
import Language.Haskell.TH (loc_filename, location)
import Language.Haskell.TH.Quote

data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          deriving (Data, Typeable, Show)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

symbol = Tok.symbol lexer
parens = Tok.parens lexer
identifier = Tok.identifier lexer
whiteSpace = Tok.whiteSpace lexer

var :: Parser Expr
var = Var <$> identifier

lam :: Parser Expr
lam = do
  symbol "\\"
  x <- identifier
  symbol "->"
  e <- expr
  return $ Lam x e

app :: Parser Expr
app = term `chainl1` return App

term :: Parser Expr
term = var <|> parens expr <?> "term"

expr :: Parser Expr
expr = lam <|> app <?> "expr"

lambda :: QuasiQuoter
lambda = QuasiQuoter
  { quoteExp = \str-> do
      -- record current position for parse error reporting
      filename <- loc_filename <$> location
      case parse (whiteSpace *> expr <* eof) filename str of
        Left err -> error (show err)
        Right e -> dataToExpQ (const Nothing) e
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = undefined
  }

{- Useage:
{-# LANGUAGE QuasiQuotes # }
import LambdaQ
e :: Expr
e = [lambda| \x -> \y -> x y |]
-}
