{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks 'LogObject's from the forwarder app).
module Trace.Forward.Acceptor
  ( runTraceAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Exception (SomeException, try)
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Network.Acceptor (listenToForwarder)
import           Trace.Forward.Configuration (AcceptorConfiguration (..))

-- | Please note that acceptor is a server from the __networking__ point of view:
-- the forwarder establishes network connection with the acceptor. This is because
-- a few forwarders are able to connect to the same acceptor.
runTraceAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor configuration.
  -> TBQueue lo               -- ^ The queue all received 'LogObject's will be write in.
  -> IO ()
runTraceAcceptor config loQueue =
  try (listenToForwarder config loQueue) >>= \case
    Left (_e :: SomeException) ->
      runTraceAcceptor config loQueue
    Right _ -> return ()
