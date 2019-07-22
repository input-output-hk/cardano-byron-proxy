{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Index.ChainDB
  ( trackChainDB
  ) where

import Control.Concurrent.Async (race)
import Control.Exception (bracket)

import Ouroboros.Byron.Proxy.Index.Types (Index)
import qualified Ouroboros.Byron.Proxy.Index.Types as Index
import Ouroboros.Consensus.Block (GetHeader (Header))
import Ouroboros.Network.Block (ChainUpdate (..), Point (..))
import Ouroboros.Network.Point (WithOrigin (Origin))
import Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

-- | Reapaetedly take the reader instruction and update the index accordingly.
trackReader
  :: ( Monad m )
  => Index m (Header blk)
  -> Reader m blk (Header blk)
  -> m x
trackReader idx reader = do
  instruction <- ChainDB.readerInstructionBlocking reader
  case instruction of
    AddBlock blk -> do
      Index.rollforward idx blk
      trackReader idx reader
    RollBack pnt -> do
      Index.rollbackward idx (getPoint pnt)
      trackReader idx reader

-- | Have an Index track a ChainDB using its Reader API for the duration of
-- some monadic action. If the ChainDB does not contain the tip of the Index,
-- then the whole index will be rebuilt.
--
-- It will spawn a thread to do the index updates. This must be the only
-- index writer. It is run by `race` with the action, so exceptions in either
-- the action or the writer thread will be re-thrown here.
trackChainDB :: forall blk t . Index IO (Header blk) -> ChainDB IO blk -> IO t -> IO t
trackChainDB idx cdb act = bracket acquireReader releaseReader $ \rdr -> do
  tipPoint <- Index.tip idx
  mPoint <- ChainDB.readerForward rdr [Point tipPoint]
  -- `readerForward` docs say that if we get `Nothing`, the next reader
  -- instruction may not be a rollback, so we'll manually roll the index
  -- back. It's assumed the read pointer will be at origin (nothing else
  -- would make sense).
  case mPoint of
    Nothing -> Index.rollbackward idx Origin
    Just _  -> pure ()
  outcome <- race (trackReader idx rdr) act
  case outcome of
    Left impossible -> impossible
    Right t -> pure t
  where
  acquireReader :: IO (Reader IO blk (Header blk))
  acquireReader = ChainDB.newHeaderReader cdb
  releaseReader :: Reader IO blk (Header blk) -> IO ()
  releaseReader = ChainDB.readerClose
