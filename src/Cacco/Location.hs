{-# LANGUAGE DeriveDataTypeable #-}

module Cacco.Location
  ( Location(..)
  , fromSourcePos
  , beginLine
  , beginColumn
  , endLine
  , endColumn
  　-- re-export
  , Pos
  ) where

import           Control.Arrow       ((&&&))
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Text.Megaparsec.Pos (Pos, SourcePos)
import qualified Text.Megaparsec.Pos as Pos

-- | The abstract data type @Location@ hints source positions.
data Location = Location
  { -- | the name of the source-file or input.
    sourceName    :: FilePath,
    -- | the line number in the source-file or input.
    beginPosition :: !(Pos, Pos),
    -- | the column number in the source-file or input.
    endPosition   :: !(Pos, Pos)
  } deriving (Eq, Typeable)

-- | Convert from two @SourcePos@ to @Location@.
fromSourcePos :: SourcePos -> SourcePos -> Location
fromSourcePos b e =
  let
    n = Pos.sourceName b
    [b', e'] = (Pos.sourceLine &&& Pos.sourceColumn) <$> [b, e]
  in
    Location n b' e'

-- | Get the beginning of line number of the location.
beginLine :: Location -> Pos
beginLine = fst . beginPosition

-- | Get the beginning of column number of the location.
beginColumn :: Location -> Pos
beginColumn = snd . beginPosition

-- | Get the end of the line number of the location.
endLine :: Location -> Pos
endLine = fst . endPosition

-- | Get the end of the column number of the location.
endColumn :: Location -> Pos
endColumn = snd . endPosition

instance Ord Location where
  compare (Location sn1 (bl1, bc1) (el1, ec1)) (Location sn2 (bl2, bc2) (el2, ec2))
    | sn1 /= sn2 = compare sn1 sn2
    | bl1 /= bl2 = compare bl1 bl2
    | bc1 /= bc2 = compare bc1 bc2
    | el1 /= el2 = compare el1 el2
    | otherwise  = compare ec1 ec2

instance Show Location where
  show (Location sn (bl, bc) (el, ec)) =
    let
      [bl', bc', el', ec'] = map show [bl, bc, el, ec]
    in
      if bl == el
        then sn <> ": " <> bl' <> "," <> bc' <> "-" <> ec'
        else sn <> ": " <> bl' <> "," <> bc' <> "-" <> el' <> "," <> ec'
