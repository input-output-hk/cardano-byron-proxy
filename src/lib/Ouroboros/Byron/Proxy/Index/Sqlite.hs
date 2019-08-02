{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Index.Sqlite
  ( withIndex
  , withIndexAuto
  , index
  ) where

import Control.Exception (Exception, throwIO)
import Crypto.Hash (digestFromByteString)
import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, Query)
import qualified Database.SQLite.Simple as Sql
import System.Directory (doesFileExist)

import qualified Cardano.Binary as Binary (unAnnotated)
import Cardano.Chain.Block (HeaderHash)
import qualified Cardano.Chain.Block as Cardano
import Cardano.Chain.Slotting (EpochSlots (..), unSlotNumber)
import Cardano.Crypto.Hashing (AbstractHash (..))

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin (..), blockPointHash, blockPointSlot)
import qualified Ouroboros.Network.Point as Point (Block (..))

import Ouroboros.Byron.Proxy.Block (Block, Header, headerHash, unByronHeaderOrEBB)
import Ouroboros.Byron.Proxy.Index.Types (Index (..))
import qualified Ouroboros.Byron.Proxy.Index.Types as Index

-- | Make an index from an SQLite connection (sqlite-simple).
-- Every `indexWrite` continuation runs in an SQLite transaction
-- (BEGIN TRANSACTION).
index :: EpochSlots -> Connection -> Index IO (Header (Block cfg))
index epochSlots conn = Index
  { Index.lookup = sqliteLookup epochSlots conn
  , tip          = sqliteTip epochSlots conn
  , rollforward  = sqliteRollforward epochSlots conn
  , rollbackward = sqliteRollbackward epochSlots conn
  }

-- | Open a new or existing SQLite database. If new, it will set up the schema.
data OpenDB where
  New      :: OpenDB
  Existing :: OpenDB

-- TODO
-- should implement a feature to reconstruct/repair the index using an
-- `ImmutableDB`: iterate from the start and insert an entry for each thing.
-- Would write that in Ouroboros.Byron.Proxy.DB though, I think.

-- | Create and use an sqlite connection to back an index in a bracket style.
-- Sets the sqlite main.synchronous pragma to 0 for faster writes.
withIndex :: EpochSlots -> OpenDB -> FilePath -> (Index IO (Header (Block cfg)) -> IO t) -> IO t
withIndex epochSlots o fp k = Sql.withConnection fp $ \conn -> do
  case o of
    New      -> Sql.withTransaction conn $ do
      createTable conn
      createIndex conn
    Existing -> pure ()
  -- This greatly improves performance.
  -- FIXME study what the drawbacks are. Should be fine, since if writes
  -- are lost, the index can still be recovered.
  Sql.execute_ conn "PRAGMA main.synchronous = 0;"
  k (index epochSlots conn)

-- | Like withDB but uses file existence check to determine whether it's new
-- or existing. If it exists, it's assumed to be an sqlite database file.
withIndexAuto :: EpochSlots -> FilePath -> (Index IO (Header (Block cfg)) -> IO t) -> IO t
withIndexAuto epochSlots fp k = doesFileExist fp >>= \b -> case b of
  True  -> withIndex epochSlots Existing fp k
  False -> withIndex epochSlots New      fp k

data IndexInternallyInconsistent where
  -- | The `Int` is less than -1
  InvalidRelativeSlot :: HeaderHash -> Int -> IndexInternallyInconsistent
  -- | The header hash for the tip is not the right size.
  InvalidHash :: ByteString -> IndexInternallyInconsistent

deriving instance Show IndexInternallyInconsistent
instance Exception IndexInternallyInconsistent

-- | The index is the relation:
--
-- +------------+---------+------+
-- | HeaderHash | EpochNo | Slot |
-- +------------+---------+------+
-- | ByteString | Word    | Int  |
-- +------------+----------------+
--
-- HeaderHash primary key, epoch and relative slot unique in the table
-- (as a pair) and there's an index on this pair.
--
-- EpochNo boundary blocks get slot number -1, thereby packing them in-between
-- the last block of the prior epoch, and the first block of the next epoch.
--
-- Forward links aren't necessary, since we can tell what the next block is
-- by using the order on epoch, slot.
--
-- We use the sqlite INTEGER for epoch and slot, but the DB stuff uses Word.
-- INTEGER can be as big as 8 bytes, which may not fit into a Word depending
-- on implementation. But since we're the only ones writing to this database,
-- and we only put in Words, it should be ok.
--
sql_create_table :: Query
sql_create_table =
  "CREATE TABLE block_index\n\
  \  ( header_hash       BLOB NOT NULL PRIMARY KEY\n\
  \  , epoch             INTEGER NOT NULL\n\
  \  , slot              INTEGER NOT NULL\n\
  \  , UNIQUE (epoch, slot)\n\
  \  );"

sql_create_index :: Query
sql_create_index =
  "CREATE INDEX epoch_slot ON block_index (epoch, slot);"

createTable :: Sql.Connection -> IO ()
createTable conn = Sql.execute_ conn sql_create_table

createIndex :: Sql.Connection -> IO ()
createIndex conn = Sql.execute_ conn sql_create_index

-- | The tip is the entry with the highest epoch and slot pair.
sql_get_tip :: Query
sql_get_tip =
  "SELECT header_hash, epoch, slot FROM block_index\
  \ ORDER BY epoch DESC, slot DESC LIMIT 1;"

sqliteTip :: EpochSlots -> Sql.Connection -> IO (WithOrigin (Point.Block SlotNo HeaderHash))
sqliteTip epochSlots conn = do
   rows :: [(ByteString, Word64, Int)] <- Sql.query_ conn sql_get_tip
   case rows of
     [] -> pure Origin
     ((hhBlob, epoch, slotInt) : _) -> do
       hh <- case digestFromByteString hhBlob of
         Just hh -> pure (AbstractHash hh)
         Nothing -> throwIO $ InvalidHash hhBlob
       offsetInEpoch :: Word64 <-
         if slotInt == -1
         then pure 0
         else if slotInt >= 0
         then pure $ fromIntegral slotInt
         else throwIO $ InvalidRelativeSlot hh slotInt
       let slotNo = SlotNo $ unEpochSlots epochSlots * epoch + offsetInEpoch
       pure $ At $ Point.Block slotNo hh

sql_get_hash :: Query
sql_get_hash =
  "SELECT epoch, slot FROM block_index\
  \ WHERE header_hash = ?;"

-- | Get epoch and slot by hash.
sqliteLookup :: EpochSlots -> Sql.Connection -> HeaderHash -> IO (Maybe SlotNo)
sqliteLookup epochSlots conn hh@(AbstractHash digest) = do
  rows :: [(Word64, Int)]
    <- Sql.query conn sql_get_hash (Sql.Only (convert digest :: ByteString))
  case rows of
    [] -> pure Nothing
    ((epoch, slotInt) : _) -> do
      offsetInEpoch :: Word64 <-
        if slotInt == -1
        then pure 0
        else if slotInt >= 0
        then pure $ fromIntegral slotInt
        else throwIO $ InvalidRelativeSlot hh slotInt
      pure $ Just $ SlotNo $ unEpochSlots epochSlots * epoch + offsetInEpoch

-- The ON CONFLICT DO NOTHING is essential. The DB into which this index points
-- may fall behind the index, for instance because of an unclean shutdown in
-- which some of the block data was not sync'd to disk. In that case, the index
-- is still "correct" under our assumption of immutability (no forks).
sql_insert :: Query
sql_insert =
  "INSERT OR ROLLBACK INTO block_index VALUES (?, ?, ?);"

sqliteRollforward
  :: EpochSlots
  -> Sql.Connection
  -> Header (Block cfg)
  -> IO ()
sqliteRollforward epochSlots conn hdr =
  Sql.execute conn sql_insert (hashBytes, epoch, slot)
  where

  AbstractHash digest = headerHash hdr

  hashBytes :: ByteString
  hashBytes = convert digest

  epoch, slot :: Int
  (epoch, slot) = case unByronHeaderOrEBB hdr of
    Left  bvd  -> (fromIntegral (Cardano.boundaryEpoch bvd), -1)
    Right hdr' ->
      fromIntegral (unSlotNumber (Binary.unAnnotated (Cardano.aHeaderSlot hdr')))
      `quotRem`
      fromIntegral (unEpochSlots epochSlots)

-- | Clear the index, in case of a rollback to origin.
sql_delete :: Query
sql_delete = "DELETE FROM block_index;"

-- | Clear the index from a given point, in case of a rollback to a non-origin
-- point.
sql_delete_from :: Query
sql_delete_from =
  "DELETE FROM block_index WHERE epoch > ? OR (epoch == ? AND slot > ?);"

-- | Roll backward to a point, deleting every entry for a block strictly
-- greater than the slot at that point. If the point is not in the database,
-- an exception is thrown (TODO).
sqliteRollbackward :: EpochSlots -> Sql.Connection -> WithOrigin (Point.Block SlotNo HeaderHash) -> IO ()
sqliteRollbackward epochSlots conn point = case point of
  -- Rolling back to origin is simple: delete everything.
  Origin    -> Sql.execute_ conn sql_delete
  -- Within a transaction, look up the hash for the point, then use its
  -- relative slot to decide what to delete. This ensures that when rolling
  -- back to a relative slot 0 block, we don't mistakenly delete the EBB.
  At bpoint -> Sql.withTransaction conn $ do
    let hh@(AbstractHash digest) = blockPointHash bpoint
        expectedEpoch, expectedSlot :: Int
        (expectedEpoch, expectedSlot) =
          fromIntegral (unSlotNo (blockPointSlot bpoint))
          `quotRem`
          fromIntegral (unEpochSlots epochSlots)
    rows :: [(Word64, Int)]
      <- Sql.query conn sql_get_hash (Sql.Only (convert digest :: ByteString))
    case rows of
      -- TODO exception?
      [] -> pure ()
      -- Check that the epoch and slot match what's expected, and then
      -- delete all rows greater than them
      ((epoch, slotInt) : _) -> do
        let matchesExpectedEpoch = epoch == fromIntegral expectedEpoch
        matchesExpectedSlot <-
          if slotInt == -1
          then pure (expectedSlot == 0)
          else if slotInt >= 0
          then pure (expectedSlot == slotInt)
          else throwIO $ InvalidRelativeSlot hh slotInt
        if matchesExpectedSlot && matchesExpectedEpoch
        then Sql.execute conn sql_delete_from (epoch, epoch, slotInt)
        -- TODO exception?
        else pure ()
