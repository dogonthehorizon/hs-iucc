module Cli
  ( run,
    CliOpts (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic (..))
import Options.Generic (Modifiers (shortNameModifier), ParseRecord (parseRecord), defaultModifiers, firstLetter, getRecord, parseRecordWithModifiers)

newtype CliOpts = Cli {file :: FilePath}
  deriving (Generic, Show)

modifiers :: Modifiers
modifiers = defaultModifiers {shortNameModifier = firstLetter}

instance ParseRecord CliOpts where
  parseRecord = parseRecordWithModifiers modifiers

run :: MonadIO m => m CliOpts
run = getRecord "The toy lisp compiler."
