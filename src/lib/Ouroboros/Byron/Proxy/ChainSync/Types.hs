{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO rename to Ouroboros.Byron.Proxy.ChainSync.Point?
module Ouroboros.Byron.Proxy.ChainSync.Types
  ( Point
  , codec
  , encodePoint
  , decodePoint
  ) where

import Control.Monad.Class.MonadST (MonadST)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Network.TypedProtocol.Codec (Codec)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano (HeaderHash)
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Crypto (ProtocolMagicId)

import Ouroboros.Byron.Proxy.Block (Block, decodeHeader, encodeHeader)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Block (SlotNo)
import qualified Ouroboros.Network.Block as Block (Point (..), decodePoint, encodePoint)
import Ouroboros.Network.Point (WithOrigin)
import qualified Ouroboros.Network.Point as Point (Block)

type Point = WithOrigin (Point.Block SlotNo Cardano.HeaderHash)

encodePoint :: Point -> CBOR.Encoding
encodePoint = Block.encodePoint Binary.toCBOR . mkPoint
  where
  mkPoint :: Point -> Block.Point (Block cfg)
  mkPoint = Block.Point

decodePoint :: CBOR.Decoder s Point
decodePoint = unMkPoint <$> Block.decodePoint Binary.fromCBOR
  where
  unMkPoint :: Block.Point (Block cfg) -> Point
  unMkPoint = Block.getPoint

codec
  :: (MonadST m)
  => ProtocolMagicId
  -> EpochSlots
  -> Codec (ChainSync (Header (Block cfg)) Point) CBOR.DeserialiseFailure m Lazy.ByteString
codec pm epochSlots = codecChainSync
  encodeHeader
  (decodeHeader epochSlots)
  encodePoint
  decodePoint
