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
  ) where

import qualified Codec.CBOR.Write as CBOR (toStrictByteString)

import qualified Pos.Chain.Block as CSL (HeaderHash)
import qualified Pos.Crypto.Hashing as Legacy (AbstractHash (..))
import Pos.DB.Class (Serialized (..), SerializedBlock)

import qualified Cardano.Chain.Block as Cardano
import Cardano.Crypto.Hashing (AbstractHash (..))

import qualified Ouroboros.Consensus.Block as Consensus (GetHeader (..))
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock (..), ByronHash (..),
                      byronHeaderHash)
import Ouroboros.Consensus.Byron.Ledger.Serialisation (encodeByronBlock)
--import Ouroboros.Consensus.Storage.Common ( EpochNo(..) )

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
