{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTSyntax #-}

{-|
Module      : Ouroboros.Byron.Proxy.Block
Description : Some block-related definitions.
-}

module Ouroboros.Byron.Proxy.Block
  ( ByronBlock (..)
  , Consensus.Header
  , toSerializedBlock
  , coerceHashFromLegacy
  , coerceHashToLegacy
  , headerHash
  , isEBB
  , checkpointOffsets
  ) where

import qualified Codec.CBOR.Write as CBOR (toStrictByteString)
import Data.Word (Word64)

import qualified Pos.Chain.Block as CSL (HeaderHash)
import qualified Pos.Crypto.Hashing as Legacy (AbstractHash (..))
import Pos.DB.Class (Serialized (..), SerializedBlock)

import qualified Cardano.Chain.Block as Cardano
import Cardano.Crypto.Hashing (AbstractHash (..))

import qualified Ouroboros.Consensus.Block as Consensus (GetHeader (..))
import Ouroboros.Consensus.Ledger.Byron (ByronBlock (..),
         ByronHash (..), encodeByronBlock, byronHeaderHash)
import Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import Ouroboros.Storage.Common (EpochNo(..))

-- For type instance HeaderHash (Header blk) = HeaderHash blk
-- Anyone who imports this module will almost certainly want that instance.
import Ouroboros.Consensus.Block ()

-- | Part of the Byron Logic layer interface requires making a serialized block,
-- which is just the block's encoding.
toSerializedBlock :: ByronBlock -> SerializedBlock
toSerializedBlock = Serialized . CBOR.toStrictByteString . encodeByronBlock

-- | Convert from a new header hash to a legacy header hash. They are
-- structurally the same, nominally different.
coerceHashToLegacy :: Cardano.HeaderHash -> CSL.HeaderHash
coerceHashToLegacy (AbstractHash digest) = Legacy.AbstractHash digest

-- | Convert from a legacy header hash to a new header hash. They are
-- structurally the same, nominally different.
coerceHashFromLegacy :: CSL.HeaderHash -> ByronHash
coerceHashFromLegacy (Legacy.AbstractHash digest) = ByronHash $ AbstractHash digest

-- | Same a `blockHash` but doesn't need `ByronGiven`.
headerHash :: Consensus.Header ByronBlock -> Cardano.HeaderHash
headerHash = unByronHash . byronHeaderHash

-- | Return @Just@ the epoch number if the block is an EBB, @Nothing@ for
-- regular blocks
isEBB :: ByronBlock -> Maybe (EpochNo, ByronHash)
isEBB blk = case byronBlockRaw blk of
    Cardano.ABOBBlock _      -> Nothing
    Cardano.ABOBBoundary ebb -> Just (epochNo, byronHash)
      where
      epochNo = EpochNo . Cardano.boundaryEpoch . Cardano.boundaryHeader $ ebb
      byronHash = ByronHash . headerHash . Consensus.getHeader $ blk

-- | Compute the offsets for use by ChainFragment.selectPoints or
-- AnchoredFragment.selectPoints for some security parameter k. It uses
-- fibonacci numbers with 0 and k as endpoints, and the duplicate 1 at the
-- start removed.
checkpointOffsets :: SecurityParam -> [Word64]
checkpointOffsets (SecurityParam k) = 0 : foldr includeK ([] {- this is never forced -}) (tail fibs)
  where
    includeK :: Word64 -> [Word64] -> [Word64]
    includeK w ws | w >= k    = [k]
                  | otherwise = w : ws
    fibs :: [Word64]
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
