module IUCC.Language.Expression
  ( ExprX (..),
    XNeg,
    XPlus,
    XMinus,
    XXExpr,
  )
where

import Prelude hiding (Int, Read)

-- | The core expressions in our language.
data ExprX x
  = IntX !Integer
  | ReadX
  | NegX !(XNeg x) (ExprX x)
  | PlusX !(XPlus x) (ExprX x) (ExprX x)
  | MinusX !(XMinus x) (ExprX x) (ExprX x)
  | ExprX !(XXExpr x)

type family XNeg x

type family XPlus x

type family XMinus x

type family XXExpr x
