{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Syntax.Position
( Position
, sourceName
, line
, column
, initPosition
) where

import           Control.DeepSeq (NFData)
import           Control.Lens    (makeLenses)
import           Data.Data       (Data)
import           Data.Monoid     ((<>))
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)

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
initPosition = Position
  { _sourceName = ""
  , _line = 1
  , _column = 1
  }

instance Show Position where
  show Position { _sourceName = n, _line = l, _column = c }
    = let
        name | n == "" = "(unknown)" | otherwise = n
      in
        "(" <> name <> ":" <> show l <> "," <> show c <> ")"
