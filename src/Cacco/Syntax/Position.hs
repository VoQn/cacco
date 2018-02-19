{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Syntax.Position where

import           Control.DeepSeq     (NFData)
import           Control.Lens        (makeLenses)
import           Data.Data           (Data)
import           Data.Monoid         ((<>))
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import           Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine,
                                      unPos)
import qualified Text.Megaparsec.Pos as Pos

-- | The abstract data type @Position@ hints source positions.
data Position = Position
  { -- | the name of the source-file or input
    _sourceName :: FilePath,
    -- | the line number in the source-file or input.
    _line       :: !Word,
    -- | the column number in the source-file or input.
    _column     :: !Word
  } deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData Position

makeLenses ''Position

-- | Constant initial position (line 1, column 1)
initPosition :: Position
initPosition = Position { _sourceName = "", _line = 1, _column = 1 }

-- | Convert from two @SourcePos@ to @Position@.
fromSourcePos :: SourcePos -> Position
fromSourcePos p =
    Position {
      _sourceName = Pos.sourceName p,
      _line = fromInt $ sourceLine p,
      _column = fromInt $ sourceColumn p
    }
  where
    fromInt = fromIntegral . unPos
    {-# INLINE fromInt #-}

instance Show Position where
  show Position { _sourceName = n, _line = l, _column = c }
    = let
        name | n == "" = "(unknown)" | otherwise = n
      in
        "(" <> name <> ":" <> show l <> "," <> show c <> ")"
