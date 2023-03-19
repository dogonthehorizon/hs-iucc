-- |
-- Module : LInt.Language
-- Description : Definitions for the L_int language.
-- Stability : stable
--
-- The abstract syntax of L_int is defined as:
--
-- type  ::= Integer
-- exp   ::= (Int int)
--            | (Prim 'read ()) | (Prim '- (exp))
--            | (Prim '+ (exp exp)) | (Prim '- (exp exp))
-- L_int ::= (Program '() exp)
module LInt.Language
  ( Expr,
    LInt (..),
    module IUCC.Language.Expression,
  )
where

import IUCC.Language.Expression

-- We have no extensions to the language because the base TTG idiom expression
-- contains everything in LInt.
data LInt'

type Expr = Expr' LInt'

type instance XNeg LInt' = NoExtField

type instance XPlus LInt' = NoExtField

type instance XMinus LInt' = NoExtField

type instance XXExpr LInt' = NoExtField

-- | A program in L_int.
data LInt = Program () Expr
