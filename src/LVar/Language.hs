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
  ( ExpVar (),
    LVar (..),
    pattern VarLit,
    pattern VarRead,
    pattern VarNeg,
    pattern VarPlus,
    pattern VarMinus,
    pattern VarVar,
  )
where

import Data.Text (Text)
import IUCC.Language.Expression (ExprX (..), XMinus, XNeg, XPlus, XXExpr)

type ExpVar = ExprX Var

data Var

type instance XNeg Var = ()

type instance XPlus Var = ()

type instance XMinus Var = ()

-- In contrast to LInt, we do want to extend this language with the notion of
-- local variables
type instance XXExpr Var = (Text, Integer)

pattern VarLit :: Integer -> ExpVar
pattern VarLit i = IntX i

pattern VarRead :: ExpVar
pattern VarRead = ReadX

pattern VarNeg :: ExpVar -> ExpVar
pattern VarNeg e = NegX () e

pattern VarPlus :: ExpVar -> ExpVar -> ExpVar
pattern VarPlus e1 e2 = PlusX () e1 e2

pattern VarMinus :: ExpVar -> ExpVar -> ExpVar
pattern VarMinus e1 e2 = MinusX () e1 e2

pattern VarVar :: Text -> Integer -> ExpVar
pattern VarVar n i <- ExprX (n, i)

-- | A program in L_int.
data LVar = Program () ExpVar
