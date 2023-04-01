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
import LInt.Language qualified as LInt
import LVar.Language qualified as LVar
import Unsafe.Coerce (unsafeCoerce)

anonymise :: LVar.ExpVar -> LInt.ExpInt
anonymise expr = case expr of
  LVar.VarLit i -> LInt.IntLit i
  LVar.VarRead -> LInt.IntRead
  LVar.VarNeg e -> LInt.IntNeg (anonymise e)
  LVar.VarPlus e e' -> LInt.IntPlus (anonymise e) (anonymise e')
  LVar.VarMinus e e' -> LInt.IntMinus (anonymise e) (anonymise e')

interpretExpr' :: MonadIO m => m Integer -> LVar.ExpVar -> m Integer
interpretExpr' readFn e =
  case e of
    LVar.VarVar _ _ -> undefined
    -- TODO is unsafeCoerce the right move here, or is there something more
    -- elegant we can do in the type system?
    expr -> LInt.interpretExpr' readFn (anonymise expr)

-- | Interpret expressions in L_int.
-- FIXME: the error message from readLn is unhelpful if this is not an int.
-- TODO: investigate how to cleanly make sure that interpret functions are the
-- same across languages.
interpretExpr :: MonadIO m => LVar.ExpVar -> m Integer
interpretExpr = interpretExpr' (liftIO readLn)

-- | Interpret a program in L_int.
interpret :: MonadIO m => LVar.LVar -> m Integer
interpret (LVar.Program _ e) = interpretExpr e
