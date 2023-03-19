-- |
-- Module : LVar.Language
-- Description : Definitions for the L_var language.
-- Stability : stable
--
-- L_var extends L_int with locally scoped variables.
--
-- type  ::= Integer
-- exp   ::= *exp from L_int* | (Var var) | (Let var exp exp)
-- L_var ::= (Program '() exp)
module LVar.Language
  ( Expr,
    LVar (..),
    XVar (..),
    module IUCC.Language.Expression,
  )
where

import Data.Text (Text)
import IUCC.Language.Expression

data LVar'

data XVar = XVar Text Integer

type Expr = Expr' LVar'

type instance XNeg LVar' = NoExtField

type instance XPlus LVar' = NoExtField

type instance XMinus LVar' = NoExtField

-- In contrast to LInt, we do want to extend this language with the notion of
-- local variables
type instance XXExpr LVar' = XVar

-- | A program in L_int.
data LVar = Program () Expr
