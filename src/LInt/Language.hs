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

-- We note explicitly that LInt has no extensions beyond the core language.
-- This means that we can ignore XXExpr in pattern matches.
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance#the-extension-constructor
type instance XXExpr LInt' = DataConCantHappen

-- | A program in L_int.
data LInt = Program () Expr
