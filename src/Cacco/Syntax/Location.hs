{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cacco.Syntax.Location (
    Location,
    sourceName,
    startLine,
    startColumn,
    endLine,
    endColumn,
    initLocation,
    fromPositions,
    toPositions,
)
where

import Control.Arrow ()
import Control.DeepSeq (NFData)
import Control.Lens (
    makeLenses,
    view,
    (&),
    (.~),
    (^.),
 )
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prettyprinter (
    Doc,
    Pretty (..),
 )

import Cacco.Syntax.Position (Position)
import qualified Cacco.Syntax.Position as P

-- | The abstract data type @Location@ hints source positions.
data Location = Location
    { _sourceName :: FilePath
    -- ^ the name of the source-file or input.
    , _startLine :: !Word
    -- ^ the start line number in the source-file or input.
    , _startColumn :: !Word
    -- ^ the start column number in the source-file or input.
    , _endLine :: !Word
    -- ^ the end line number in the source-file or input.
    , _endColumn :: !Word
    -- ^ the end column number in the source-file or input.
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Location

makeLenses ''Location

-- | Constant initial location (startLine 1, startColumn 1, endLine 1, endColumn 1)
initLocation :: Location
initLocation =
    Location
        { _sourceName = ""
        , _startLine = 1
        , _startColumn = 1
        , _endLine = 1
        , _endColumn = 1
        }

fromPositions :: Position -> Position -> Location
fromPositions a b
    | srcA /= srcB = error "Can not merge two positions from different sources"
    | otherwise = location
  where
    srcA, srcB :: FilePath
    srcA = view P.sourceName a
    srcB = view P.sourceName b
    start, end :: Position
    start = min a b
    end = max a b
    location :: Location
    location =
        initLocation
            & sourceName .~ (start ^. P.sourceName)
            & startLine .~ (start ^. P.line)
            & startColumn .~ (start ^. P.column)
            & endLine .~ (end ^. P.line)
            & endColumn .~ (end ^. P.column)
    {-# INLINE location #-}

toPositions :: Location -> (Position, Position)
toPositions l = (start, end)
  where
    src :: FilePath
    src = l ^. sourceName
    {-# INLINE src #-}
    start :: Position
    start =
        P.initPosition
            & P.sourceName
                .~ src
            & P.line
                .~ l
                    ^. startLine
            & P.column
                .~ l
                    ^. startColumn
    {-# INLINE start #-}
    end :: Position
    end =
        P.initPosition
            & P.sourceName
                .~ src
            & P.line
                .~ l
                    ^. endLine
            & P.column
                .~ l
                    ^. endColumn
    {-# INLINE end #-}

instance Pretty Location where
    pretty Location{..} =
        "(" <> name _sourceName <> ":" <> start <> "-" <> end <> ")"
      where
        name :: FilePath -> Doc ann
        name [] = "(unknown)"
        name s = pretty s
        {-# INLINE name #-}
        pos :: (Pretty p) => p -> p -> Doc ann
        pos l' c' = pretty l' <> "," <> pretty c'
        {-# INLINE pos #-}
        start :: Doc ann
        start = pos _startLine _startColumn
        {-# INLINE start #-}
        end :: Doc ann
        end = pos _endLine _endColumn
        {-# INLINE end #-}
