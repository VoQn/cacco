{-# LANGUAGE DeriveDataTypeable #-}

module Cacco.Location
  ( Location(..)
  , fromSourcePos
  , beginLine
  , beginColumn
  , endLine
  , endColumn
  , Pos -- re-export
  ) where

import           Control.Arrow       ((&&&))
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Text.Megaparsec.Pos (Pos, SourcePos)
import qualified Text.Megaparsec.Pos as Pos

data Location = Location {
    sourceName    :: FilePath,
    beginPosition :: !(Pos, Pos),
    endPosition   :: !(Pos, Pos)
  } deriving (Eq, Typeable)

fromSourcePos :: SourcePos -> SourcePos -> Location
fromSourcePos b e =
  let
    n = Pos.sourceName b
    [b', e'] = (Pos.sourceLine &&& Pos.sourceColumn) <$> [b, e]
  in
    Location n b' e'

beginLine :: Location -> Pos
beginLine = fst . beginPosition

beginColumn :: Location -> Pos
beginColumn = snd . beginPosition

endLine :: Location -> Pos
endLine = fst . endPosition

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
