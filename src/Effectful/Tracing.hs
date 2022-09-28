{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Tracing
  ( -- * Effect
    Tracing
  , MonadTrace (..)

    -- * Handlers
  , runTrace

    -- * Functions
  , rootSpan
  , childSpan
  , clientSpan
  , clientSpanWith
  , serverSpan
  , serverSpanWith
  , producerSpanWith
  , consumerSpanWith
  , tag
  , annotate
  , annotateAt
  )
where

import Control.Monad.Trace
import Effectful
import Effectful.Dispatch.Static
import Monitor.Tracing

data Tracing :: Effect

type instance DispatchOf Tracing = Static WithSideEffects
newtype instance StaticRep Tracing = Tracing ()

runTrace
  :: IOE :> es
  => Eff (Tracing : es) a
  -> Tracer
  -> Eff es a
runTrace action tracer = evalStaticRep (Tracing ())

----------------------------------------
-- Orphan instance

deriving via (TraceT m) instance Tracing :> es => MonadTrace (Eff es)
