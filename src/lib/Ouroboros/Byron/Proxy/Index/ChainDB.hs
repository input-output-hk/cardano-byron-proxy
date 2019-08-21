{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Index.ChainDB
  ( trackChainDB
  ) where

import Control.Exception (bracket)

import Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.Types as Index
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import Ouroboros.Network.Block (ChainUpdate (..), Point (..))
import Ouroboros.Network.Point (WithOrigin (Origin))
import Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Reapaetedly take the reader instruction and update the index accordingly.
trackReaderBlocking
  :: ( Monad m )
  => Index m (Header blk)
  -> Reader m blk (Header blk)
  -> m x
trackReaderBlocking idx reader = do
  instruction <- ChainDB.readerInstructionBlocking reader
  case instruction of
    AddBlock blk -> do
      Index.rollforward idx blk
      trackReaderBlocking idx reader
    RollBack pnt -> do
      Index.rollbackward idx (getPoint pnt)
      trackReaderBlocking idx reader

-- | Do reader instructions to update the index, until there is no more
-- instruction.
trackReader
  :: ( Monad m )
  => Index m (Header blk)
  -> Reader m blk (Header blk)
  -> m ()
trackReader idx reader = do
  mInstruction <- ChainDB.readerInstruction reader
  case mInstruction of
    Just (AddBlock blk) -> do
      Index.rollforward idx blk
      trackReader idx reader
    Just (RollBack pnt) -> do
      Index.rollbackward idx (getPoint pnt)
      trackReader idx reader
    Nothing -> pure ()

-- | Have an Index track a ChainDB using its Reader API. You probably want to
-- race this with some other thread that runs your application.
--
-- If the ChainDB does not contain the tip of the Index, then the whole index
-- will be rebuilt.
--
-- It will spawn a thread to do the index updates. This must be the only
-- index writer. It is run by `race` with the action, so exceptions in either
-- the action or the writer thread will be re-thrown here.
--
-- If the tip of the index is not in the ChainDB, then the entire index will be
-- rebuilt. This is not ideal: there may be an intersection. TODO would be
-- better to check the newest slot older than `k` back from tip of index, and
-- go from there.
trackChainDB
  :: forall blk void .
     ResourceRegistry IO
  -> Index IO (Header blk)
  -> ChainDB IO blk
  -> IO void
trackChainDB rr idx cdb = bracket acquireReader releaseReader $ \rdr -> do
  tipPoint <- Index.tip idx
  mPoint <- ChainDB.readerForward rdr [Point tipPoint]
  -- `readerForward` docs say that if we get `Nothing`, the next reader
  -- instruction may not be a rollback, so we'll manually roll the index
  -- back. It's assumed the read pointer will be at origin (nothing else
  -- would make sense).
  case mPoint of
    Nothing -> Index.rollbackward idx Origin
    Just _  -> pure ()
  -- First, block until the index is caught up to the tip ...
  trackReader idx rdr
  -- ... then attempt to stay in sync.
  trackReaderBlocking idx rdr
  where
  acquireReader :: IO (Reader IO blk (Header blk))
  acquireReader = ChainDB.newHeaderReader cdb rr
  releaseReader :: Reader IO blk (Header blk) -> IO ()
  releaseReader = ChainDB.readerClose
