module IUCC.Language.Expression
  ( Expr' (..),
    XNeg,
    XPlus,
    XMinus,
    XXExpr,
    NoExtField (..),
  )
where

import Prelude hiding (Int, Read)

-- | The core expressions in our language.
data Expr' x
  = Int Integer
  | Read
  | Neg (XNeg x) (Expr' x)
  | Plus (XPlus x) (Expr' x) (Expr' x)
  | Minus (XMinus x) (Expr' x) (Expr' x)
  | XExpr !(XXExpr x)

type family XNeg x

type family XPlus x

type family XMinus x

type family XXExpr x

-- | A compiler phase/language does not have an extension field.
data NoExtField = NoExtField
