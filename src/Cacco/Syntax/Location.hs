{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Cacco.Syntax.Location
  ( Location
  , sourceName
  , startLine
  , startColumn
  , endLine
  , endColumn
  , initLocation
  , fromPositions
  , toPostions
  ) where

import           Control.Arrow             ((&&&))
import           Control.DeepSeq           (NFData)
import           Control.Lens              (makeLenses, view, (&), (.~), (^.))
import           Data.Text.Prettyprint.Doc
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import           Cacco.Syntax.Position     (Position)
import qualified Cacco.Syntax.Position     as P

-- | The abstract data type @Location@ hints source positions.
data Location = Location
  { -- | the name of the source-file or input.
    _sourceName  :: FilePath,
    -- | the start line number in the source-file or input.
    _startLine   :: !Word,
    -- | the start column number in the source-file or input.
    _startColumn :: !Word,
    -- | the end line number in the source-file or input.
    _endLine     :: !Word,
    -- | the end column number in the source-file or input.
    _endColumn   :: !Word
  } deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Location

makeLenses ''Location

-- | Constant initial location (startLine 1, startColumn 1, endLine 1, endColumn 1)
initLocation :: Location
initLocation =
  Location {
    _sourceName = "",
    _startLine = 1,
    _startColumn = 1,
    _endLine = 1,
    _endColumn = 1
  }

fromPositions :: Position -> Position -> Location
fromPositions a b
    | srcA /= srcB = error "Can not merge two positions from different sources"
    | otherwise    = location
  where
    srcA, srcB :: FilePath
    [srcA, srcB] = view P.sourceName <$> [a, b]
    start, end :: Position
    (start, end) = uncurry min &&& uncurry max $ (a, b)
    location :: Location
    location = initLocation
      & sourceName .~ start ^. P.sourceName
      & startLine .~ start ^. P.line
      & startColumn .~ start ^. P.column
      & endLine .~  end ^. P.line
      & endColumn .~ end ^.  P.column
    {-# INLINE location #-}

toPostions :: Location -> (Position, Position)
toPostions l = (start, end)
  where
    src :: FilePath
    src = l ^. sourceName
    {-# INLINE src #-}
    start :: Position
    start = P.initPosition
        & P.sourceName .~ src
        & P.line .~ l ^. startLine
        & P.column .~ l ^. startColumn
    {-# INLINE start #-}
    end :: Position
    end = P.initPosition
        & P.sourceName .~ src
        & P.line .~ l ^. endLine
        & P.column .~ l ^. endColumn
    {-# INLINE end #-}

instance Pretty Location where
  pretty Location{..} = parens $ surround colon name $ surround hyphen start end
    where
      name :: Doc ann
      name
        | _sourceName == "" = "(unknown)"
        | otherwise = pretty _sourceName
      start :: Doc ann
      start = surround comma (pretty _startLine) $ pretty _startColumn
      end :: Doc ann
      end = surround comma (pretty _endLine) $ pretty _endColumn
      hyphen :: Doc ann
      hyphen = "-"
