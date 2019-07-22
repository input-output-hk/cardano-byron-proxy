{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Client
  ( Options (..)
  , runClient
  ) where

import Codec.SerialiseTerm (encodeTerm, decodeTerm)
import qualified Network.Socket as Network
import Control.Monad.Class.MonadSTM (atomically)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant (contramap)
import qualified Data.Text.Lazy.Builder as Text

import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import Cardano.Crypto (ProtocolMagicId)

import Ouroboros.Byron.Proxy.Block (Block, unByronHeaderOrEBB)
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import Ouroboros.Byron.Proxy.ChainSync.Types (Point)
import Ouroboros.Byron.Proxy.Network.Protocol (initiatorVersions)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Network.Socket (connectToNode)
import qualified Ouroboros.Network.Block as Block (Point (..))
import Ouroboros.Storage.ChainDB.API (ChainDB, getTipPoint)

import Orphans ()

data Options = Options
  { hostName          :: !Network.HostName
    -- ^ Of remote peer
  , serviceName       :: !Network.ServiceName
    -- ^ Of remote peer
  }

-- | Run a Shelley chain sync client which downloads, writes to a database,
-- and prints. It stops after any rollback, because the DB is at the moment
-- immutable (no rollbacks).
runClient
  :: forall cfg .
     Options
  -> Tracer IO Text.Builder
  -> ProtocolMagicId
  -> Cardano.EpochSlots
  -> ChainDB IO (Block cfg)
  -> IO ()
runClient options tracer pm epochSlots db = do
  addrInfosLocal  <- Network.getAddrInfo (Just addrInfoHints) (Just "127.0.0.1") (Just "0")
  addrInfosRemote <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  case (addrInfosLocal, addrInfosRemote) of
    (addrInfoLocal : _, addrInfoRemote : _) -> connectToNode
      encodeTerm
      decodeTerm
      -- TODO: this should be some proper type rather than a tuple
      (,)
      (initiatorVersions pm epochSlots chainSyncClient)
      (Just addrInfoLocal)
      addrInfoRemote
    _ -> error "no getAddrInfo"
  where
  host = hostName options
  port = serviceName options
  addrInfoHints = Network.defaultHints
  -- | This chain sync client will first try to improve the read pointer to
  -- the tip of the database, and then will roll forward forever, stopping
  -- if there is a roll-back.
  -- It makes sense given that we only have an immutable database and one
  -- source for blocks: one read pointer improve is always enough.
  chainSyncClient = Client.chainSyncClient fold
    where
    fold :: Client.Fold cfg IO ()
    fold = Client.Fold $ do
      Block.Point tip <- atomically $ getTipPoint db
      pure $ Client.Improve [tip] $ \_ _ -> roll
    roll :: Client.Fold cfg IO ()
    roll = Client.Fold $ pure $ Client.Continue forward backward
    forward :: Header (Block cfg) -> Point -> Client.Fold cfg IO ()
    forward hdr point = Client.Fold $ do
      traceWith (contramap chainSyncShow tracer) (Right hdr, point)
      -- TODO do this using block fetch.
      -- What to do with the header though?
      -- addBlock db blk
      Client.runFold roll
    backward :: Point -> Point -> Client.Fold cfg IO ()
    backward point1 point2 = Client.Fold $ do
      traceWith (contramap chainSyncShow tracer) (Left point1, point2)
      Client.runFold roll

  chainSyncShow
    :: (Either Point (Header (Block cfg)), Point)
    -> Text.Builder
  chainSyncShow = \(roll, _tip) -> case roll of
    Left  back    -> mconcat
      [ Text.fromString "Roll back to\n"
      , Text.fromString (show back)
      ]
    Right forward -> mconcat
      [ Text.fromString "Roll forward to\n"
      , case unByronHeaderOrEBB forward of
          Left  ebb -> Text.fromString (show ebb)
          Right hdr -> Cardano.renderHeader
            epochSlots
            (fmap (const ()) hdr)
      ]
