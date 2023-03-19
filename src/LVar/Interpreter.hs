-- |
-- Module : LVar.Interpreter
-- Description : Implementation for the L_var language embedded in Haskell.
-- Stability : stable
module LVar.Interpreter
  ( interpret,
    interpretExpr',
    interpretExpr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import LInt.Interpreter qualified as LInt
import LVar.Language qualified as LVar
import Unsafe.Coerce (unsafeCoerce)

interpretExpr' :: MonadIO m => m Integer -> LVar.Expr -> m Integer
interpretExpr' readFn e =
  case e of
    LVar.XExpr _ -> undefined
    -- TODO is unsafeCoerce the right move here, or is there something more
    -- elegant we can do in the type system?
    _ -> LInt.interpretExpr' readFn (unsafeCoerce e)

-- | Interpret expressions in L_int.
-- FIXME: the error message from readLn is unhelpful if this is not an int.
-- TODO: investigate how to cleanly make sure that interpret functions are the
-- same across languages.
interpretExpr :: MonadIO m => LVar.Expr -> m Integer
interpretExpr = interpretExpr' (liftIO readLn)

-- | Interpret a program in L_int.
interpret :: MonadIO m => LVar.LVar -> m Integer
interpret (LVar.Program _ e) = interpretExpr e
