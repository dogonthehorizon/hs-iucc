module Main (main) where

import Cli (CliOpts (..))
import Cli qualified
import LInt.Interpreter qualified as LInt
import LInt.Parser qualified as LInt
import Text.Megaparsec qualified as M

main :: IO ()
main =
  -- TODO eventually parametrize the language used by the compiler.
  Cli.run >>= LInt.parse . Cli.file >>= \case
    Left errBundle -> putStr $ M.errorBundlePretty errBundle
    Right ast -> LInt.interpret ast >>= print
