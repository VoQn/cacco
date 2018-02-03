{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Location
  ( Location(..)
  , sourceName
  , startLine
  , startColumn
  , endLine
  , endColumn
  , initLocation
  , fromSourcePos
  , toPostions
  ) where

import           Cacco.Position      (Position)
import qualified Cacco.Position      as P
import           Control.Lens        (makeLenses, (&), (.~), (^.))
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Text.Megaparsec.Pos (SourcePos)
-- import qualified Text.Megaparsec.Pos as Pos

-- | The abstract data type @Location@ hints source positions.
data Location = Location
  { -- | the name of the source-file or input.
    _sourceName  :: FilePath,
    -- | the start line number in the source-file or input.
    _startLine   :: Word,
    -- | the start column number in the source-file or input.
    _startColumn :: Word,
    -- | the end line number in the source-file or input.
    _endLine     :: Word,
    -- | the end column number in the source-file or input.
    _endColumn   :: Word
  } deriving (Eq, Typeable)

makeLenses ''Location

initLocation :: Location
initLocation = Location{
  _sourceName = "",
  _startLine = 1,
  _startColumn = 1,
  _endLine = 1,
  _endColumn = 1
  }

-- | Convert from two @SourcePos@ to @Location@.
fromSourcePos :: SourcePos -> SourcePos -> Location
fromSourcePos s e =
  let
    startPos = P.fromSourcePos s
    endPos = P.fromSourcePos e
  in
    Location {
      _sourceName = startPos ^. P.sourceName,
      _startLine = startPos ^. P.line,
      _startColumn = startPos ^. P.column,
      _endLine = endPos ^. P.line,
      _endColumn = endPos ^. P.column
      }

toPostions :: Location -> (Position, Position)
toPostions Location {
    _sourceName  = n,
    _startLine   = sl,
    _startColumn = sc,
    _endLine     = el,
    _endColumn   = ec
  } =
  let
    s = P.initPosition
        & P.sourceName .~ n
        & P.line .~ sl
        & P.column .~ sc
    e = P.initPosition
        & P.sourceName .~ n
        & P.line .~ el
        & P.column .~ ec
  in (s, e)

instance Show Location where
  show Location {
    _sourceName  = n,
    _startLine   = sl,
    _startColumn = sc,
    _endLine     = el,
    _endColumn   = ec
  } =
    let
      n' = if n == "" then "(unknown)" else n
    in
      "(" <> n' <> ":" <> show sl <> "," <> show sc <> "-" <> show el <> "," <> show ec <> ")"
