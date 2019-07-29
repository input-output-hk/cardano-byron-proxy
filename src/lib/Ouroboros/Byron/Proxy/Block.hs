{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTSyntax #-}

module Ouroboros.Byron.Proxy.Block
  ( Block
  , ByronBlockOrEBB (..)
  , Consensus.Header (ByronHeaderOrEBB, unByronHeaderOrEBB)
  , toSerializedBlock
  , coerceHashFromLegacy
  , coerceHashToLegacy
  , headerHash
  , isEBB
  ) where

import qualified Data.ByteString.Lazy as Lazy

import qualified Pos.Chain.Block as CSL (HeaderHash)
import qualified Pos.Crypto.Hashing as Legacy (AbstractHash (..))
import Pos.DB.Class (Serialized (..), SerializedBlock)

import qualified Cardano.Chain.Block as Cardano
import Cardano.Crypto.Hashing (AbstractHash (..))

import qualified Ouroboros.Consensus.Block as Consensus (GetHeader (..))
import Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..),
         pattern ByronHeaderOrEBB, blockBytes, unByronHeaderOrEBB)

-- For type instance HeaderHash (Header blk) = HeaderHash blk
-- Anyone who imports this module will almost certainly want that instance.
import Ouroboros.Consensus.Block ()

type Block cfg = ByronBlockOrEBB cfg

toSerializedBlock :: Block cfg -> SerializedBlock
toSerializedBlock = Serialized . Lazy.toStrict . blockBytes

-- TODO: Move these functions to a compatibility module
coerceHashToLegacy :: Cardano.HeaderHash -> CSL.HeaderHash
coerceHashToLegacy (AbstractHash digest) = Legacy.AbstractHash digest

coerceHashFromLegacy :: CSL.HeaderHash -> Cardano.HeaderHash
coerceHashFromLegacy (Legacy.AbstractHash digest) = AbstractHash digest

-- | Same a `blockHash` but doesn't need `ByronGiven`.
headerHash :: Consensus.Header (Block cfg) -> Cardano.HeaderHash
headerHash hdr = case unByronHeaderOrEBB hdr of
  Left ebb -> Cardano.boundaryHeaderHashAnnotated ebb
  Right mh -> Cardano.headerHashAnnotated mh

isEBB :: Block cfg -> Maybe Cardano.HeaderHash
isEBB blk = case unByronBlockOrEBB blk of
  Cardano.ABOBBlock _    -> Nothing
  Cardano.ABOBBoundary b -> Just $ headerHash (Consensus.getHeader blk)
