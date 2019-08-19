{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Index.ChainDB
  ( trackChainDB
  ) where

import Control.Concurrent.Async (race)
import Control.Exception (bracket)
import Data.Word (Word64)

import Ouroboros.Byron.Proxy.Block (checkpointOffsets)
import Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.Types as Index
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import Ouroboros.Network.Block (ChainUpdate (..), Point (..))
import Ouroboros.Network.Point (WithOrigin (Origin), block)
import Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Reapaetedly take the reader instruction and update the index accordingly.
trackReaderBlocking
  :: ( Monad m )
  => Index m (Header blk)
  -> Reader m blk (Header blk)
  -> m void
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

-- | Have an Index track a ChainDB using its Reader API for the duration of
-- some monadic action.
--
-- It will spawn a thread to do the index updates. This must be the only
-- index writer. It is run by `race` with the action, so exceptions in either
-- the action or the writer thread will be re-thrown here.
--
-- If the tip of the index is in the ChainDB, then no work must be done in the
-- beginning. But if it's not in the ChainDB, there will have to be a rollback
-- on the index. The SecurityParam k is used to decide how far back to try. If
-- Only index entries at most k slots old will be checked against the
-- ChainDB. If none are in it, then the entire index will be rebuild (rollback
-- to Origin).
trackChainDB
  :: forall blk t .
     ResourceRegistry IO
  -> Index IO (Header blk)
  -> ChainDB IO blk
  -> SecurityParam
  -> IO t
  -> IO t
trackChainDB rr idx cdb k act = bracket acquireReader releaseReader $ \rdr -> do
    checkpoints <- Index.streamFromTip idx checkpointsFold
    mPoint <- ChainDB.readerForward rdr checkpoints
    case mPoint of
      -- `readerForward` docs say that the next instruction will be a rollback,
      -- so we don't have to do anything here; the call to `trackReader` will
      -- do what needs to be done.
      Just _  -> pure ()
      -- `readerForward` docs say that if we get `Nothing`, the next reader
      -- instruction may not be a rollback, so we'll manually roll the index
      -- back. It's assumed the read pointer will be at origin (nothing else
      -- would make sense).
      Nothing -> Index.rollbackward idx Origin
    -- First, block until the index is caught up to the tip ...
    trackReader idx rdr
    -- ... then attempt to stay in sync.
    outcome <- race (trackReaderBlocking idx rdr) act
    case outcome of
      Left impossible -> impossible
      Right t -> pure t
  where
  acquireReader :: IO (Reader IO blk (Header blk))
  acquireReader = ChainDB.deserialiseReader <$> ChainDB.newHeaderReader cdb rr
  releaseReader :: Reader IO blk (Header blk) -> IO ()
  releaseReader = ChainDB.readerClose

  checkpointsFold :: Index.Fold (Header blk) [Point blk]
  checkpointsFold = checkpointsFoldN 0 (checkpointOffsets k)

  -- Count up from 0 on the first parameter. Whenever it coincides with the
  -- head of the second parameter (an increasing list) include that point.
  -- Stop when the second list is empty.
  -- Since checkpointsFold always includes the paramater k, the k'th entry
  -- in the index will always be in here, unless the index is shorter
  -- than k. This block is _at least_ k slots behind the DB, so if it's not
  -- in the DB then the index is way out of date.
  checkpointsFoldN
    :: Word64
    -> [Word64]
    -> Index.Fold (Header blk) [Point blk]
  checkpointsFoldN _ []       = Index.Stop []
  checkpointsFoldN w (o : os) = Index.More [] $ \slotNo hash ->
    if w == o
    then fmap ((:) (Point (block slotNo hash))) (checkpointsFoldN (w+1) os)
    else checkpointsFoldN (w+1) (o : os)
