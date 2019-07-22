{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Network.Protocol where

import qualified Codec.CBOR.Term as CBOR
import Codec.Serialise (Serialise)
import Codec.SerialiseTerm (CodecCBORTerm (..))
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Word (Word16)
import Data.Void (Void)

import qualified Cardano.Chain.Slotting as Cardano
import Cardano.Crypto (ProtocolMagicId)

import Network.TypedProtocol.Driver (runPeer)
import Network.Mux.Types (MiniProtocolLimits (..), ProtocolEnum (..))
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Network.Mux (AppType (..), OuroborosApplication (..))
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient, chainSyncClientPeer)
import Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer, chainSyncServerPeer)
import Ouroboros.Network.Protocol.Handshake.Version

import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Byron.Proxy.ChainSync.Types (Point)
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync (codec)

-- | Version number for handshake. Needs Show and Serialise in order to be
-- useful.
newtype VNumber = VNumber
  { getVNumber :: Word16
  } deriving (Show, Eq, Ord, Enum, Serialise)

unitCodecCBORTerm :: CodecCBORTerm Text ()
unitCodecCBORTerm = CodecCBORTerm
  { encodeTerm = const CBOR.TNull
  , decodeTerm = \term -> case term of
      CBOR.TNull -> Right ()
      _          -> Left "expected TNull"
  }

-- | Protocol enum for muxing. Also needs Show for some reason.
data Ptcl where
  PtclChainSync :: Ptcl
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Required in order to be useful. Why do I also need Enum?
instance ProtocolEnum Ptcl where
  fromProtocolEnum PtclChainSync = 1024
  toProtocolEnum word = case word of
    1024 -> Just PtclChainSync
    _ -> Nothing

-- | Required in order to be useful.
instance MiniProtocolLimits Ptcl where
  maximumMessageSize  = const 0xffffffff
  maximumIngressQueue = const 0xffffffff

initiatorVersions
  :: ( Monad m, MonadST m, MonadUnliftIO m, MonadThrow m )
  => ProtocolMagicId
  -> Cardano.EpochSlots -- ^ Needed for the codec, sadly
  -> ChainSyncClient (Header (Block cfg)) Point m ()
  -> Versions VNumber (CodecCBORTerm Text) (OuroborosApplication InitiatorApp peer Ptcl m LBS.ByteString () Void)
initiatorVersions pm epochSlots client = Versions $ Map.fromList
  [ (VNumber 0, Sigma () (Version clientMuxApp unitCodecCBORTerm))
  ]
  where
  clientPeer = chainSyncClientPeer client
  codec = ChainSync.codec pm epochSlots
  clientMuxApp = Application $ \_ _ -> OuroborosInitiatorApplication $ \peer ptcl channel -> case ptcl of
    PtclChainSync -> runPeer nullTracer codec peer channel clientPeer

responderVersions
  :: ( Monad m, MonadST m, MonadUnliftIO m, MonadThrow m )
  => ProtocolMagicId
  -> Cardano.EpochSlots -- ^ Needed for the codec; must match that of the initiator.
  -> ChainSyncServer (Header (Block cfg)) Point m ()
  -> Versions VNumber (CodecCBORTerm Text) (OuroborosApplication ResponderApp peer Ptcl m LBS.ByteString Void ())
responderVersions pm epochSlots server = Versions $ Map.fromList
  [ (VNumber 0, Sigma () (Version serverMuxApp unitCodecCBORTerm))
  ]
  where
  serverPeer = chainSyncServerPeer server
  codec = ChainSync.codec pm epochSlots
  serverMuxApp = Application $ \_ _ -> OuroborosResponderApplication $ \peer ptcl channel -> case ptcl of
    PtclChainSync -> runPeer nullTracer codec peer channel serverPeer
