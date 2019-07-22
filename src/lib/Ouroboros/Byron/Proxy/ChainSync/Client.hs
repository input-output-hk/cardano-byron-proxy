{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.ChainSync.Client where

import Ouroboros.Network.Protocol.ChainSync.Client

import Ouroboros.Consensus.Block (Header)
import Ouroboros.Byron.Proxy.Block (Block)
import Ouroboros.Byron.Proxy.ChainSync.Types (Point)

newtype Fold cfg m a = Fold
  { runFold :: m (Next cfg m a)
  }

data Next cfg m a where
  Stop     :: a      -> Next cfg m a
  -- | In the continuation, `Just` means it was improved, `Nothing` means
  -- none of the points were found. The second `Point` is the current tip.
  Improve  :: [Point] -> (Maybe Point -> Point -> Fold cfg m a) -> Next cfg m a
  Continue :: (Header (Block cfg) -> Point -> Fold cfg m a) -- Roll forawrd
           -> (Point              -> Point -> Fold cfg m a) -- Roll backward
           -> Next cfg m a

-- | Repeatedly request next, and run the `Fold` until it finishes.
chainSyncClient
  :: forall cfg m a .
     ( Monad m )
  => Fold cfg m a
  -> ChainSyncClient (Header (Block cfg)) Point m a
chainSyncClient fold = ChainSyncClient $ do
  next <- runFold fold
  case next of
    Stop a -> pure $ SendMsgDone a
    Improve pts k -> pure $ SendMsgFindIntersect pts $ ClientStIntersect
      { recvMsgIntersectImproved  = \rp tip -> chainSyncClient (k (Just rp) tip)
      , recvMsgIntersectUnchanged = \tip    -> chainSyncClient (k Nothing tip)
      }
    Continue forward backward -> pure $ SendMsgRequestNext immediate later
      where
      immediate :: ClientStNext (Header (Block cfg)) Point m a
      immediate = ClientStNext
        { recvMsgRollForward = \blk point -> chainSyncClient (forward blk point)
        , recvMsgRollBackward = \point1 point2 -> chainSyncClient (backward point1 point2)
        }
      later :: m (ClientStNext (Header (Block cfg)) Point m a)
      later = pure immediate
