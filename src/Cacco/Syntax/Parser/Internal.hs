module Cacco.Syntax.Parser.Internal
  (
    Parser
  , ParseError
  ) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import qualified Text.Megaparsec as MP

type Parser = MP.Parsec Void Text
type ParseError = MP.ParseError (MP.Token Text) Void
