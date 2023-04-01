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
  ( ExpInt (),
    LInt (..),
    pattern IntLit,
    pattern IntRead,
    pattern IntNeg,
    pattern IntPlus,
    pattern IntMinus,
  )
where

import Data.Void (Void)
import IUCC.Language.Expression (ExprX (..), XMinus, XNeg, XPlus, XXExpr)

-- We have no extensions to the language because the base TTG idiom expression
-- contains everything in LInt.
type ExpInt = ExprX I

data I

type instance XNeg I = ()

type instance XPlus I = ()

type instance XMinus I = ()

-- We note explicitly that LInt has no extensions beyond the core language.
-- This means that we can ignore XXExpr in pattern matches.
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance#the-extension-constructor
-- Further reading here indicates that we can use Void and () to make this simpler:
-- https://vaibhavsagar.com/blog/2018/06/19/trees-that-shrink/
type instance XXExpr ExpInt = Void

pattern IntLit :: Integer -> ExpInt
pattern IntLit i = IntX i

pattern IntRead :: ExpInt
pattern IntRead = ReadX

pattern IntNeg :: ExpInt -> ExpInt
pattern IntNeg e = NegX () e

pattern IntPlus :: ExpInt -> ExpInt -> ExpInt
pattern IntPlus e1 e2 = PlusX () e1 e2

pattern IntMinus :: ExpInt -> ExpInt -> ExpInt
pattern IntMinus e1 e2 = MinusX () e1 e2

-- | A program in L_int.
data LInt = Program () ExpInt
