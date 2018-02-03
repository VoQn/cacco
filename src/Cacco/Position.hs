{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Position where

import           Control.Lens        (makeLenses, (^.))
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Pos as Pos

-- | The abstract data type @Position@ hints source positions.
data Position = Position
  { -- | the name of the source-file or input.
    _sourceName :: FilePath,
    -- | the line number in the source-file or input.
    _line       :: Word,
    -- | the column number in the source-file or input.
    _column     :: Word
  } deriving (Eq, Typeable)

makeLenses ''Position

initPosition :: Position
initPosition = Position { _sourceName = "", _line = 1, _column = 1}

-- | Convert from two @SourcePos@ to @Position@.
fromSourcePos :: SourcePos -> Position
fromSourcePos p =
  let
    n = Pos.sourceName p
    l = fromIntegral $ Pos.unPos $ Pos.sourceLine p
    c = fromIntegral $ Pos.unPos $ Pos.sourceColumn p
  in
    Position { _sourceName = n, _line = l, _column = c }

instance Ord Position where
  compare Position{_sourceName = n1, _line = l1, _column = c1}
          Position{_sourceName = n2, _line = l2, _column = c2}
    | n1 /= n2  = n1 `compare` n2
    | l1 /= l2  = l1 `compare` l2
    | otherwise = c1 `compare` c2

instance Show Position where
  show x =
    let
      n = x ^. sourceName
      n' = if n == "" then "(unknown)" else n
      l = show $ x ^. line
      c = show $ x ^. column
    in
      "(" <> n' <> ":" <> l <> "," <> c <> ")"
