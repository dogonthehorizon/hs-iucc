-- |
-- Module : LInt.Interpreter
-- Description : Implementation for the L_int language embedded in Haskell.
-- Stability : stable
module LInt.Interpreter
  ( interpret,
    interpretExpr',
    interpretExpr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import LInt.Language qualified as LInt

interpretExpr' :: MonadIO m => m Integer -> LInt.Expr -> m Integer
interpretExpr' readFn e =
  case e of
    LInt.Int' a -> return a
    LInt.Read' -> readFn
    LInt.Neg e' -> negate <$> interpretExpr e'
    LInt.Plus e1 e2 -> do
      e1' <- interpretExpr e1
      e2' <- interpretExpr e2
      return $ e1' + e2'
    LInt.Minus e1 e2 -> do
      e1' <- interpretExpr e1
      e2' <- interpretExpr e2
      return $ e1' - e2'

-- | Interpret expressions in L_int.
-- FIXME: the error message from readLn is unhelpful if this is not an int.
interpretExpr :: MonadIO m => LInt.Expr -> m Integer
interpretExpr = interpretExpr' (liftIO readLn)

-- | Interpret a program in L_int.
interpret :: MonadIO m => LInt.LInt -> m Integer
interpret (LInt.Program _ e) = interpretExpr e
