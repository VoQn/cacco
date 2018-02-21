{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cacco.Env
  ( Env(..)
  , initEnv
  , newEnv
  , findFrame
  , get
  , push
  ) where

import           Control.Monad.Except (MonadError, throwError)
import           Data.Map             (Map, (!?))
import qualified Data.Map             as Map

import           Cacco.Error          (Error (..))

data Env a = Env
  { outer   :: Maybe (Env a)
  , symbols :: Map String a
  } deriving (Eq)

newEnv :: Maybe (Env a) -> Env a
newEnv outerEnv = Env { outer = outerEnv, symbols = Map.empty }

initEnv :: Env a
initEnv = newEnv Nothing

findFrame :: Env a -> String -> Maybe (Env a)
findFrame env key = case Map.lookup key (symbols env) of
  Just _ -> Just env
  Nothing -> case outer env of
    Nothing -> Nothing
    Just o  -> findFrame o key

get :: MonadError (Error i) m => Env a -> String -> m a
get env key = case findFrame env key of
    Nothing -> throwError $ Message ("'" ++ key ++ "' not found.") Nothing
    Just scope -> case Map.lookup key (symbols scope) of
      Nothing  -> throwError $ Message ("'" ++ key ++ "' not found") Nothing
      Just val -> return val

push :: (MonadError (Error i) m) => Env a -> String -> a -> m (Env a)
push env k v = do
  let currentTable = symbols env
  case currentTable !? k of
    Just _ -> throwError $ Message ("Symbol '" ++ k ++ "' was already bound") Nothing
    Nothing -> do
      let tbl' = Map.insert k v $ symbols env
      return $ env { symbols = tbl' }
