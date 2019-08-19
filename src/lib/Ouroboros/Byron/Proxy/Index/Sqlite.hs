{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Ouroboros.Byron.Proxy.Index.Sqlite
  ( withIndex
  , withIndexAuto
  , index
  , TraceEvent (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Tracer (Tracer, traceWith)
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

import Ouroboros.Consensus.Ledger.Byron (ByronHash(..),
         byronHeaderSlotNo, byronHeaderRaw)
import Ouroboros.Consensus.Ledger.Byron.Auxiliary as Cardano

import Ouroboros.Byron.Proxy.Block (ByronBlock(..), Header,
         headerHash)
import Ouroboros.Byron.Proxy.Index.Types (Index (..))
import qualified Ouroboros.Byron.Proxy.Index.Types as Index

data TraceEvent where
  Rollback    :: WithOrigin (Point.Block SlotNo ByronHash) -> TraceEvent
  Rollforward :: Point.Block SlotNo ByronHash              -> TraceEvent
  deriving (Show)

-- | Make an index from an SQLite connection (sqlite-simple).
index
  :: EpochSlots
  -> Tracer IO TraceEvent
  -> Connection
  -> Sql.Statement -- for insert
  -> Index IO (Header ByronBlock)
index epochSlots tracer conn insertStatement = Index
  { Index.lookup  = sqliteLookup epochSlots conn
  , tip           = sqliteTip epochSlots conn
  , streamFromTip = sqliteStreamFromTip epochSlots conn
  , rollforward   = sqliteRollforward epochSlots tracer insertStatement
  , rollbackward  = sqliteRollbackward epochSlots tracer conn
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
withIndex
  :: EpochSlots
  -> Tracer IO TraceEvent
  -> OpenDB
  -> FilePath
  -> (Index IO (Header ByronBlock) -> IO t)
  -> IO t
withIndex epochSlots tracer o fp k = Sql.withConnection fp $ \conn -> do
  case o of
    New      -> Sql.withTransaction conn $ do
      createTable conn
      createIndex conn
    Existing -> pure ()
  -- This greatly improves performance.
  -- FIXME study what the drawbacks are. Should be fine, since if writes
  -- are lost, the index can still be recovered.
  Sql.execute_ conn "PRAGMA main.synchronous = 0;"
  -- Since insert will probably be hammered, we'll use a prepared statement
  -- for it.
  Sql.withStatement conn sql_insert $ \insertStatement ->
    k (index epochSlots tracer conn insertStatement)

-- | Like withDB but uses file existence check to determine whether it's new
-- or existing. If it exists, it's assumed to be an sqlite database file.
withIndexAuto
  :: EpochSlots
  -> Tracer IO TraceEvent
  -> FilePath
  -> (Index IO (Header ByronBlock) -> IO t)
  -> IO t
withIndexAuto epochSlots tracer fp k = doesFileExist fp >>= \b -> case b of
  True  -> withIndex epochSlots tracer Existing fp k
  False -> withIndex epochSlots tracer New      fp k

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

convertHashBlob :: ByteString -> IO HeaderHash
convertHashBlob blob = case digestFromByteString blob of
  Just hh -> pure (AbstractHash hh)
  Nothing -> throwIO $ InvalidHash blob

-- | Convert the database encoding of relative slot to the offset in an
-- epoch. The header hash is taken for error-reporting purposes.
offsetInEpoch :: HeaderHash -> Int -> IO Word64
offsetInEpoch hh i
  | i == -1 = pure 0
  | i >= 0  = pure $ fromIntegral i
  | otherwise = throwIO $ InvalidRelativeSlot hh i

toAbsoluteSlot :: EpochSlots -> HeaderHash -> Word64 -> Int -> IO SlotNo
toAbsoluteSlot epochSlots hh epochNo slotInt = do
  offset <- offsetInEpoch hh slotInt
  pure $ SlotNo $ unEpochSlots epochSlots * epochNo + offset

-- | The tip is the entry with the highest epoch and slot pair.
sql_get_tip :: Query
sql_get_tip =
  "SELECT header_hash, epoch, slot FROM block_index\
  \ ORDER BY epoch DESC, slot DESC LIMIT 1;"

sqliteTip :: EpochSlots -> Sql.Connection -> IO (WithOrigin (Point.Block SlotNo ByronHash))
sqliteTip epochSlots conn = do
   rows :: [(ByteString, Word64, Int)] <- Sql.query_ conn sql_get_tip
   case rows of
     [] -> pure Origin
     ((hhBlob, epoch, slotInt) : _) -> do
       hh <- convertHashBlob hhBlob
       slotNo <- toAbsoluteSlot epochSlots hh epoch slotInt
       pure $ At $ Point.Block slotNo (ByronHash hh)

sql_get_all :: Query
sql_get_all =
  "SELECT header_hash, epoch, slot FROM block_index\
  \ ORDER BY epoch DESC, slot DESC;"

-- | Stream rows from the tip by using the prepared statement and nextRow
-- API.
sqliteStreamFromTip
  :: EpochSlots
  -> Sql.Connection
  -> Index.Fold (Header ByronBlock) t
  -> IO t
sqliteStreamFromTip epochSlots conn fold = Sql.withStatement conn sql_get_all $ \stmt ->
    go stmt fold
  where
    go stmt step = case step of
      Index.Stop t -> pure t
      Index.More t k -> do
        next <- Sql.nextRow stmt
        case next of
          Nothing -> pure t
          Just (hhBlob, epoch, slotInt) -> do
            hh <- convertHashBlob hhBlob
            slotNo <- toAbsoluteSlot epochSlots hh epoch slotInt
            go stmt (k slotNo (ByronHash hh))

sql_get_hash :: Query
sql_get_hash =
  "SELECT epoch, slot FROM block_index\
  \ WHERE header_hash = ?;"

-- | Get epoch and slot by hash.
sqliteLookup :: EpochSlots -> Sql.Connection -> ByronHash -> IO (Maybe SlotNo)
sqliteLookup epochSlots conn (ByronHash hh@(AbstractHash digest)) = do
  rows :: [(Word64, Int)]
    <- Sql.query conn sql_get_hash (Sql.Only (convert digest :: ByteString))
  case rows of
    [] -> pure Nothing
    ((epoch, slotInt) : _) -> do
      slotNo <- toAbsoluteSlot epochSlots hh epoch slotInt
      pure $ Just slotNo

-- | Note that there is a UNIQUE constraint. This will fail if a duplicate
-- entry is inserted.
sql_insert :: Query
sql_insert =
  "INSERT OR ROLLBACK INTO block_index VALUES (?, ?, ?);"

sqliteRollforward
  :: EpochSlots
  -> Tracer IO TraceEvent
  -> Sql.Statement -- the insert prepared statement
  -> Header ByronBlock
  -> IO ()
sqliteRollforward epochSlots tracer insertStatement hdr = do
  traceWith tracer (Rollforward point)
  Sql.withBind insertStatement (hashBytes, epoch, slot) $ do
    outcome <- Sql.nextRow insertStatement
    case outcome of
      Nothing -> pure ()
      -- sqlite-simple has a problem with prepared statements that do not
      -- return rows. We still have to choose a type with a FromRow instance,
      -- even though we should not get any rows. `Only Word64` is chosen,
      -- somewhat arbitrarily.
      -- https://github.com/nurpax/sqlite-simple/issues/50
      Just (Sql.Only (_ :: Word64)) ->
        error "sqliteRollforward: row returned from insert"

  where

  point = Point.Block slotNo (ByronHash hh)

  hh@(AbstractHash digest) = headerHash hdr

  hashBytes :: ByteString
  hashBytes = convert digest

  slotNo :: SlotNo
  slotNo = byronHeaderSlotNo hdr

  epoch, slot :: Int
  (epoch, slot) = case byronHeaderRaw hdr of
    Cardano.ABOBBoundaryHdr bvd  -> (fromIntegral (Cardano.boundaryEpoch bvd), -1)
    Cardano.ABOBBlockHdr    hdr' ->
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
sqliteRollbackward
  :: EpochSlots
  -> Tracer IO TraceEvent
  -> Sql.Connection
  -> WithOrigin (Point.Block SlotNo ByronHash)
  -> IO ()
sqliteRollbackward epochSlots tracer conn point = do
  traceWith tracer (Rollback point)
  case point of
    -- Rolling back to origin is simple: delete everything.
    Origin    -> Sql.execute_ conn sql_delete
    -- Within a transaction, look up the hash for the point, then use its
    -- relative slot to decide what to delete. This ensures that when rolling
    -- back to a relative slot 0 block, we don't mistakenly delete the EBB.
    At bpoint -> Sql.withTransaction conn $ do
      let ByronHash hh@(AbstractHash digest) = blockPointHash bpoint
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
