-- |
-- Module : LInt.Parser
-- Description : Parser combinators for the L_int language.
-- Stability : stable
--
-- The concrete syntax of L_int is defined as:
--
-- type  ::= Integer
-- exp   ::= int | (read) | (- exp) | (+ exp exp) | (- exp exp)
-- L_int ::= exp
module LInt.Parser (parse) where

import Control.Applicative (empty, liftA2, optional)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Void (Void)
import LInt.Language qualified as LInt
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, eof, runParser, try)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parenthesized :: Parser a -> Parser a
parenthesized = between (symbol "(") (symbol ")")

pInt :: Parser LInt.ExpInt
pInt = LInt.IntLit <$> lexeme L.decimal

pRead :: Parser LInt.ExpInt
pRead = LInt.IntRead <$ parenthesized (symbol "read")

pPlus :: Parser LInt.ExpInt
pPlus =
  parenthesized $ do
    _ <- symbol "+"
    e1 <- pExpr
    e2 <- pExpr
    return $ LInt.IntPlus e1 e2

pMinus :: Parser LInt.ExpInt
pMinus =
  parenthesized $ do
    _ <- symbol "-"
    e1 <- pExpr
    (optional . try $ pExpr)
      >>= \case
        -- We have a minus operation
        Just e2 -> return $ LInt.IntMinus e1 e2
        -- Otherwise unary argument is negation
        Nothing -> return $ LInt.IntNeg e1

pExpr :: Parser LInt.ExpInt
pExpr =
  choice
    [ pInt,
      -- Since the grammar overlaps we need to introduce a 'try' to enable
      -- backtracking.
      try pRead,
      try pPlus,
      pMinus
    ]

pProgram :: Parser LInt.LInt
pProgram = LInt.Program () <$> pExpr

parse :: MonadIO m => FilePath -> m (Either (ParseErrorBundle Text Void) LInt.LInt)
parse fName = do
  f <- liftIO $ T.readFile fName
  return $ runParser (spaceConsumer *> pProgram <* eof) fName f
