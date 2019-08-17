{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cacco.Syntax.Index.Series
  ()
where

import           Test.SmallCheck.Series

import           Cacco.Syntax.Index

instance Monad m => Serial m Index

instance Monad m => Serial m (IndexProxy 'Expr) where
  series = cons0 ExprProxy

instance Monad m => Serial m (IndexProxy 'Decl) where
  series = cons0 DeclProxy

instance Monad m => Serial m (IndexProxy 'Patt) where
  series = cons0 PattProxy

instance Monad m => Serial m (IndexProxy 'Type) where
  series = cons0 TypeProxy
