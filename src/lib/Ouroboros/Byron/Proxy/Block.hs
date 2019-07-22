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
  , blockBytes
  , encodeBlock
  , decodeBlock
  , encodeHeader
  , decodeHeader
  , toSerializedBlock
  , coerceHashFromLegacy
  , coerceHashToLegacy
  , headerHash
  , isEBB
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString.Lazy as Lazy

import qualified Pos.Chain.Block as CSL (HeaderHash)
import qualified Pos.Crypto.Hashing as Legacy (AbstractHash (..))
import Pos.DB.Class (Serialized (..), SerializedBlock)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Crypto.Hashing (AbstractHash (..))

import qualified Ouroboros.Consensus.Block as Consensus (GetHeader (..))
import Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..),
         pattern ByronHeaderOrEBB, unByronHeaderOrEBB, fromCBORAHeaderOrBoundary)

-- For type instance HeaderHash (Header blk) = HeaderHash blk
-- Anyone who imports this module will almost certainly want that instance.
import Ouroboros.Consensus.Block ()

type Block cfg = ByronBlockOrEBB cfg

-- Codec for blocks and headers using CBOR-in-CBOR. This style is needed
-- because the cardano-ledger decoders drop information that must be
-- retained if we are to communicate with Byron peers.
-- Re-annotating in the cardano-ledger style, i.e. re-encoding then using those
-- bytes to annotate, does not work, because information is lost in the
-- initial decoding.
-- This codec must be used for network exchange _and_ for the database.

blockBytes :: Block cfg -> Lazy.ByteString
blockBytes blk = Lazy.pack [listLengthByte, discriminatorByte] `Lazy.append` Lazy.fromStrict mainBytes
  where
  -- Here's how it would look if we could use CBOR encoding.
  --
  --  enc = CBOR.encodeListLen 2 <> blockBytes
  --  blockEnc = case unByronBlockOrEBB blk of
  --    Cardano.ABOBBoundary b -> toCBOR (0 :: Word) <> toCBOR
  --    Cardano.ABOBBlock b    -> toCBOR (1 :: Word) <> toCBORBlock epochSlots b
  --
  -- But since we don't carry an annotation for the _entire_ block, and we
  -- can't put a bytestring representing a _part_ of the encoding into an
  -- encoding, we have to work directly with bytes. Here's how it goes:
  --
  --   0b100_00010 for list length (major type 4) value 2
  --   0b000_0000x where x is 1 for EBB, 0 for main block. Unsigned integer (major type 0)
  --
  -- No binary literals so we'll write them hex.
  listLengthByte = 0x82
  discriminatorByte = case unByronBlockOrEBB blk of
    Cardano.ABOBBoundary _ -> 0x00
    Cardano.ABOBBlock    _ -> 0x01
  mainBytes = case unByronBlockOrEBB blk of
    Cardano.ABOBBlock    b -> Cardano.blockAnnotation b
    Cardano.ABOBBoundary b -> Cardano.boundaryBlockBytes b

encodeBlock :: Block cfg -> CBOR.Encoding
encodeBlock = Binary.encodeUnknownCborDataItem . blockBytes

decodeBlock :: EpochSlots -> CBOR.Decoder s (Block cfg)
decodeBlock epochSlots = do
  bytes <- Binary.decodeUnknownCborDataItem
  case Binary.decodeFullAnnotatedBytes "Block" internalDecoder (Lazy.fromStrict bytes) of
    Right it  -> pure $ ByronBlockOrEBB it
    -- FIXME
    --   err :: Binary.DecodeError
    -- but AFAICT the only way to make the decoder fail is to give a `String`
    -- to `fail`...
    Left  err -> fail (show err)
  where
  internalDecoder :: Binary.Decoder s (Cardano.ABlockOrBoundary Binary.ByteSpan)
  internalDecoder = Cardano.fromCBORABlockOrBoundary epochSlots

headerBytes :: Consensus.Header (Block cfg) -> Lazy.ByteString
headerBytes blk = Lazy.pack [listLengthByte, discriminatorByte] `Lazy.append` Lazy.fromStrict mainBytes
  where
  listLengthByte = 0x82
  discriminatorByte = case unByronHeaderOrEBB blk of
    Left  _ -> 0x00
    Right _ -> 0x01
  mainBytes = case unByronHeaderOrEBB blk of
    Left  ebb -> Cardano.boundaryHeaderBytes ebb
    Right hdr -> Cardano.headerAnnotation hdr

encodeHeader :: Consensus.Header (Block cfg) -> CBOR.Encoding
encodeHeader = Binary.encodeUnknownCborDataItem . headerBytes

decodeHeader :: EpochSlots -> CBOR.Decoder s (Consensus.Header (Block cfg))
decodeHeader epochSlots = do
  bytes <- Binary.decodeUnknownCborDataItem
  -- Would use decodeFullAnnotatedBytes, but we can't because it only works for
  -- an f ByteSpan
  case Binary.decodeFullAnnotatedBytes "Header" internalDecoder (Lazy.fromStrict bytes) of
    Right (LeftF ebb)  -> pure $ ByronHeaderOrEBB (Left ebb)
    Right (RightF hdr) -> pure $ ByronHeaderOrEBB (Right hdr)
    Left  err          -> fail (show err)
  where
  internalDecoder :: Binary.Decoder s (EitherF Cardano.BoundaryValidationData Cardano.AHeader Binary.ByteSpan)
  internalDecoder = fromEither <$> fromCBORAHeaderOrBoundary epochSlots

-- | Defined only for use by decodeHeader.
data EitherF g h t where
  LeftF  :: g t -> EitherF g h t
  RightF :: h t -> EitherF g h t

instance (Functor g, Functor h) => Functor (EitherF g h) where
  fmap f (LeftF g)  = LeftF  (fmap f g)
  fmap f (RightF h) = RightF (fmap f h)

fromEither :: Either (g t) (h t) -> EitherF g h t
fromEither (Left g)  = LeftF g
fromEither (Right h) = RightF h

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
  Left ebb -> Cardano.boundaryHashAnnotated ebb
  Right mh -> Cardano.headerHashAnnotated mh

isEBB :: Block cfg -> Maybe Cardano.HeaderHash
isEBB blk = case unByronBlockOrEBB blk of
  Cardano.ABOBBlock _    -> Nothing
  Cardano.ABOBBoundary b -> Just $ headerHash (Consensus.getHeader blk)
