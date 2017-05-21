{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module Cacco.Literal
  ( Location(..)
  , Literal(..)
  , getLocation
  ) where

import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Typeable   (Typeable)
import           Text.Megaparsec (Pos)

data Location = Location {
    sourceName  :: FilePath,
    beginLine   :: !Pos,
    beginColumn :: !Pos,
    endLine     :: !Pos,
    endColumn   :: !Pos
  } deriving (Eq, Typeable)

instance Ord Location where
  compare (Location sn1 bl1 bc1 el1 ec1) (Location sn2 bl2 bc2 el2 ec2)
    | sn1 /= sn2 = compare sn1 sn2
    | bl1 /= bl2 = compare bl1 bl2
    | bc1 /= bc2 = compare bc1 bc2
    | el1 /= el2 = compare el1 el2
    | otherwise  = compare ec1 ec2

instance Show Location where
  show (Location sn bl bc el ec) =
    let
      [bl', bc', el', ec'] = map show [bl, bc, el, ec]
    in if bl == el
      then sn <> ": " <> bl' <> "," <> bc' <> "-" <> ec'
      else sn <> ": " <> bl' <> "," <> bc' <> "-" <> el' <> "," <> ec'

-- |Literal Data for cacco
data Literal
  = IntegerLiteral Integer    Location
  | DecimalLiteral Scientific Location
  | StringLiteral  String     Location
  deriving (Eq, Ord, Typeable)

instance Show Literal where
  show = \case
    IntegerLiteral x l -> "Integer " <> show x <> " (" <> show l <> ")"
    DecimalLiteral x l -> "Decimal " <> show x <> " (" <> show l <> ")"
    StringLiteral  x l -> "String "  <> show x <> " (" <> show l <> ")"

getLocation :: Literal -> Location
getLocation = \case
  IntegerLiteral _ l -> l
  DecimalLiteral _ l -> l
  StringLiteral  _ l -> l
