module Cacco.Syntax.Parser.Internal
  ( Parser
  , ParseError
  , locationFromSourcePos
  ) where

import           Control.Lens          ((&), (.~))
import           Data.Text             (Text)
import           Data.Void             (Void)
import           Text.Megaparsec       (Parsec, SourcePos, Token)
import qualified Text.Megaparsec       as MP
import qualified Text.Megaparsec.Pos   as Pos

import           Cacco.Syntax.Location (Location, fromPositions)
import           Cacco.Syntax.Position (Position, initPosition)
import qualified Cacco.Syntax.Position as P

type Parser = Parsec Void Text
type ParseError = MP.ParseError (Token Text) Void

-- | Convert from two @SourcePos@ to @Position@.
fromSourcePos :: SourcePos -> Position
fromSourcePos p =
    initPosition
      & P.sourceName .~ src
      & P.line .~ line
      & P.column .~ column
  where
    src :: FilePath
    src = Pos.sourceName p
    {-# INLINE src #-}
    line :: Word
    line = fromInt $ Pos.sourceLine p
    {-# INLINE line #-}
    column :: Word
    column = fromInt $ Pos.sourceColumn p
    {-# INLINE column #-}
    fromInt :: Pos.Pos -> Word
    fromInt = fromIntegral . Pos.unPos
    {-# INLINE fromInt #-}

-- | Convert from two @SourcePos@ to @Location@.
locationFromSourcePos :: SourcePos -> SourcePos -> Location
locationFromSourcePos p1' p2' = fromPositions p1 p2
  where
    p1, p2 :: Position
    [p1, p2] = fromSourcePos <$> [p1', p2']
