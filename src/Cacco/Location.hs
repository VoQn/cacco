{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Location
  ( Location(..)
  , HasLocation(..)
  , sourceName
  , line
  , column
  , initLocation
  , fromSourcePos
  ) where

import           Control.Lens        (makeLenses, (^.))
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos

-- | The abstract data type @Location@ hints source positions.
data Location = Location
  { -- | the name of the source-file or input.
    _sourceName :: FilePath,
    -- | the line number in the source-file or input.
    _line       :: Word,
    -- | the column number in the source-file or input.
    _column     :: Word
  } deriving (Eq, Typeable)

makeLenses ''Location

class HasLocation a where
  getLocation :: a -> Location

instance HasLocation Location where
  getLocation = id

initLocation :: Location
initLocation = Location{ _sourceName = "", _line = 1, _column = 1}

-- | Convert from two @SourcePos@ to @Location@.
fromSourcePos :: SourcePos -> Location
fromSourcePos p =
  let
    n = Pos.sourceName p
    l = Pos.unPos $ Pos.sourceLine p
    c = Pos.unPos $ Pos.sourceColumn p
  in
    Location { _sourceName = n, _line = l, _column = c }

instance Ord Location where
  compare Location{_sourceName = n1, _line = l1, _column = c1}
          Location{_sourceName = n2, _line = l2, _column = c2}
    | n1 /= n2  = n1 `compare` n2
    | l1 /= l2  = l1 `compare` l2
    | otherwise = c1 `compare` c2

instance Show Location where
  show location =
    let
      n = location ^. sourceName
      l = show $ location ^. line
      c = show $ location ^. column
    in
      (if n == "" then "(unknown)" else n) <> ": " <> l <> "," <> c
