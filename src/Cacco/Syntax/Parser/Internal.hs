module Cacco.Syntax.Parser.Internal
  ( Parser
  , ParseError
  , locationFromSourcePos
  ) where

import           Control.Lens
import           Data.Text             (Text)
import           Data.Void             (Void)
import qualified Text.Megaparsec       as MP
import qualified Text.Megaparsec.Pos   as Pos

import           Cacco.Syntax.Location (Location, initLocation)
import qualified Cacco.Syntax.Location as L
import           Cacco.Syntax.Position (Position, initPosition)
import qualified Cacco.Syntax.Position as P

type Parser = MP.Parsec Void Text
type ParseError = MP.ParseError (MP.Token Text) Void

-- | Convert from two @SourcePos@ to @Position@.
positionFromSourcePos :: MP.SourcePos -> Position
positionFromSourcePos p =
    initPosition
      & P.sourceName .~ Pos.sourceName p
      & P.line .~ fromInt (Pos.sourceLine p)
      & P.column .~ fromInt (Pos.sourceColumn p)
  where
    fromInt = fromIntegral . Pos.unPos
    {-# INLINE fromInt #-}

-- | Convert from two @SourcePos@ to @Location@.
locationFromSourcePos
  :: MP.SourcePos -- ^ start position
  -> MP.SourcePos -- ^ end position
  -> Location
locationFromSourcePos s e =
  let
    s' = positionFromSourcePos s
    e' = positionFromSourcePos e
  in
    initLocation
      & L.sourceName .~ s' ^. P.sourceName
      & L.startLine .~ s' ^. P.line
      & L.startColumn .~ s' ^. P.column
      & L.endLine .~ e' ^. P.line
      & L.endColumn .~ e' ^. P.column
