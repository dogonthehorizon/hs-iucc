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
  ( Expr (..),
    LInt (..),
  )
where

-- | Expressions in L_int.
data Expr
  = Int' Integer
  | Read'
  | Neg Expr
  | Plus Expr Expr
  | Minus Expr Expr
  deriving (Show)

-- | A program in L_int.
data LInt = Program () Expr
  deriving (Show)
