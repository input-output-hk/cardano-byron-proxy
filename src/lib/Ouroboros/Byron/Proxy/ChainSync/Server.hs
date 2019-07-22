{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.ChainSync.Server
  ( chainSyncServer
  ) where

import Control.Monad.Class.MonadSTM (MonadSTM, atomically)

import Ouroboros.Byron.Proxy.Block (Block (..))
import Ouroboros.Byron.Proxy.ChainSync.Types (Point)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Block (ChainUpdate (..))
import qualified Ouroboros.Network.Block as Block (Point (..))
import Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Use a `ChainDB` `Reader` on headers to serve through `ChainSync`.
chainSyncServer
  :: forall m hdr cfg .
     ( Monad m
     , MonadSTM m
     )
  => ChainDB m (Block cfg)
  -> ChainDB.Reader m (Block cfg) (Header (Block cfg))
  -> ChainSyncServer (Header (Block cfg)) Point m ()
chainSyncServer db reader = ChainSyncServer $ pure $ ServerStIdle
  { recvMsgDoneClient    = pure ()
  , recvMsgFindIntersect = \points -> do
      mPoint <- ChainDB.readerForward reader (fmap Block.Point points)
      Block.Point tipPoint <- atomically $ ChainDB.getTipPoint db
      case mPoint of
        Nothing                  -> pure $ SendMsgIntersectUnchanged tipPoint (chainSyncServer db reader)
        Just (Block.Point point) -> pure $ SendMsgIntersectImproved point tipPoint (chainSyncServer db reader)
  , recvMsgRequestNext = do
      mInstruction <- ChainDB.readerInstruction reader
      case mInstruction of
        Just instruction -> Left <$> fromReaderInstruction instruction
        Nothing -> pure $ Right $ do
          instruction <- ChainDB.readerInstructionBlocking reader
          fromReaderInstruction instruction
  }
  where
  fromReaderInstruction
    :: ChainUpdate (Block cfg) (Header (Block cfg))
    -> m (ServerStNext (Header (Block cfg)) Point m ())
  fromReaderInstruction update = do
    Block.Point tipPoint <- atomically $ ChainDB.getTipPoint db
    case update of
      AddBlock hdr ->
        pure $ SendMsgRollForward hdr tipPoint (chainSyncServer db reader)
      RollBack (Block.Point point) ->
        pure $ SendMsgRollBackward point tipPoint (chainSyncServer db reader)
