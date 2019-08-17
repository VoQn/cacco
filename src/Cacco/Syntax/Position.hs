{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Cacco.Syntax.Position
  ( Position
  , sourceName
  , line
  , column
  , initPosition
  )
where

import           Control.DeepSeq                ( NFData )
import           Control.Lens                   ( makeLenses )
import           Data.Data                      ( Data )
import           Data.Text.Prettyprint.Doc      ( Doc
                                                , Pretty(..)
                                                , (<>)
                                                )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )


-- | The abstract data type @Position@ hints source positions.
data Position = Position
    { _sourceName :: FilePath
    -- ^ the name of the source-file or input
    , _line       :: !Word
    -- ^ the line number in the source-file or input.
    , _column     :: !Word
    -- ^ the column number in the source-file or input.
    } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Position

makeLenses ''Position

-- | Constant initial position (line 1, column 1)
initPosition :: Position
initPosition = Position { _sourceName = "", _line = 1, _column = 1 }

instance Pretty Position where
  pretty Position {..} =
    "("
    <> name _sourceName
    <> ":"
    <> pretty _line
    <> ","
    <> pretty _column
    <> ")"
   where
    name :: FilePath -> Doc ann
    name [] = "(unknown)"
    name s  = pretty s
    {-# INLINE name #-}
