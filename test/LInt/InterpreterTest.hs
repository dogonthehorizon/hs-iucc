module LInt.InterpreterTest where

import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LInt.Interpreter qualified as LInt
import LInt.Language qualified as LInt
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.constantFrom 0 (-10000) 10000)

test_interpInt :: TestTree
test_interpInt = testProperty
  "interpreting int should be idempotent"
  $ property
  $ do
    i <- forAll genInteger
    i' <- LInt.interpretExpr $ LInt.Int' i
    i === i'

test_interpNeg :: TestTree
test_interpNeg = testProperty
  "interpreting negation should always negate the argument"
  $ property
  $ do
    i <- forAll genInteger
    i' <- LInt.interpretExpr $ LInt.Neg (LInt.Int' i)
    (-i) === i'

test_interpPlus :: TestTree
test_interpPlus = testProperty
  "interpreting addition should always add the arguments together"
  $ property
  $ do
    i <- forAll genInteger
    j <- forAll genInteger
    r <- LInt.interpretExpr $ LInt.Plus (LInt.Int' i) (LInt.Int' j)
    (i + j) === r

test_interpMinus :: TestTree
test_interpMinus = testProperty
  "interpreting subtraction should always sub the arguments together"
  $ property
  $ do
    i <- forAll genInteger
    j <- forAll genInteger
    r <- LInt.interpretExpr $ LInt.Minus (LInt.Int' i) (LInt.Int' j)
    (i - j) === r

test_interpRead :: TestTree
test_interpRead = testProperty
  "interpreting reads should always return the read integer"
  $ property
  $ do
    i <- forAll genInteger
    r <- LInt.interpretExpr' (pure i) LInt.Read'
    i === r
