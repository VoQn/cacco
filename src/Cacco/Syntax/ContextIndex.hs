{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Syntax.ContextIndex where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import           Data.Proxy      (Proxy (..))
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)

-- | Index of the context about Statement, Expression, and Term.
data ContextIndex
    = ExpressionContext
    | DeclarationContext
    | PatternContext
    | TypingContext
    deriving (Eq, Show, Data, Typeable, Generic)

instance NFData ContextIndex

type ExpressionContext = 'ExpressionContext
type DeclarationContext = 'DeclarationContext
type PatternContext = 'PatternContext
type TypingContext = 'TypingContext

pattern ExprProxy :: forall (ctx :: ContextIndex). ctx ~ 'ExpressionContext => Proxy ctx
pattern ExprProxy = Proxy

pattern DeclProxy :: forall (ctx :: ContextIndex). ctx ~ 'DeclarationContext => Proxy ctx
pattern DeclProxy = Proxy

pattern PattProxy :: forall (ctx :: ContextIndex). ctx ~ 'PatternContext => Proxy ctx
pattern PattProxy = Proxy

pattern TypeProxy :: forall (ctx :: ContextIndex). ctx ~ 'TypingContext => Proxy ctx
pattern TypeProxy = Proxy
