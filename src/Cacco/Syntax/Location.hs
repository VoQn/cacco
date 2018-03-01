{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Syntax.Location
  ( Location
  , sourceName
  , startLine
  , startColumn
  , endLine
  , endColumn
  , initLocation
  , toPostions
  ) where

import           Control.DeepSeq       (NFData)
import           Control.Lens          (makeLenses, (&), (.~), (^.))
import           Data.Monoid           ((<>))
import           Data.Typeable         (Typeable)
import           GHC.Generics

import           Cacco.Syntax.Position (Position)
import qualified Cacco.Syntax.Position as P

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
  } deriving (Eq, Ord, Typeable, Generic)

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

toPostions :: Location -> (Position, Position)
toPostions l =
  let
    n = l ^. sourceName
    s = P.initPosition
        & P.sourceName .~ n
        & P.line .~ l ^. startLine
        & P.column .~ l ^. startColumn
    e = P.initPosition
        & P.sourceName .~ n
        & P.line .~ l ^. endLine
        & P.column .~ l ^. endColumn
  in
    (s, e)

instance Show Location where
  show Location {
    _sourceName  = n,
    _startLine   = sl,
    _startColumn = sc,
    _endLine     = el,
    _endColumn   = ec
  } =
    let
      name | n == "" = "(unknown)" | otherwise = n
    in
      "("
      <> name <> ":"
      <> show sl <> "," <> show sc
      <> "-"
      <> show el <> "," <> show ec
      <> ")"
