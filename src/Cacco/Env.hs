{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cacco.Env (
    Ambient (..),
    Env,
    initAmb,
    newAmb,
    find,
    hasKeyHere,
    hasKeySomeWhere,
    lookup,
    register,
)
where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust)
import Prelude hiding (lookup)

data Ambient k a = Ambient
    { outerAmbient :: Maybe (Ambient k a)
    , localScope :: Map k a
    }
    deriving (Eq, Show)

instance Functor (Ambient k) where
    fmap f amb =
        Ambient
            { outerAmbient = fmap f <$> outerAmbient amb
            , localScope = f <$> localScope amb
            }

type Env = Ambient String

initAmb :: Ambient k a
initAmb = Ambient{outerAmbient = Nothing, localScope = Map.empty}

newAmb :: Ambient k a -> Ambient k a
newAmb amb = initAmb{outerAmbient = Just amb}

find :: (Ord k) => k -> Ambient k a -> Maybe (a, Ambient k a)
find key ambient = case Map.lookup key (localScope ambient) of
    Just entry -> Just (entry, ambient)
    Nothing -> case outerAmbient ambient of
        Nothing -> Nothing
        Just outer -> find key outer

hasKeySomeWhere :: (Ord k) => k -> Ambient k a -> Bool
hasKeySomeWhere key = isJust . find key

hasKeyHere :: (Ord k) => k -> Ambient k a -> Bool
hasKeyHere key = Map.member key . localScope

lookup :: (Ord k) => k -> Ambient k a -> Maybe a
lookup key = fmap fst . find key

register :: (Ord k) => k -> a -> Ambient k a -> Ambient k a
register key value ambient =
    let registered = Map.insert key value $ localScope ambient
     in ambient{localScope = registered}
