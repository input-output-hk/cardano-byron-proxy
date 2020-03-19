{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Byron.Proxy.Genesis.Convert where

import Data.Coerce (coerce)
import qualified Data.Fixed as Fixed (resolution)
import qualified Data.Map as Map (fromList)
import qualified Data.HashMap.Strict as HashMap (toList)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word16)
import Numeric.Natural (Natural)
import GHC.Real ((%))

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Common as Cardano
import qualified Cardano.Chain.Delegation as Cardano
import qualified Cardano.Chain.Delegation (annotation, signature)
import qualified Cardano.Chain.Genesis as Cardano
import qualified Cardano.Chain.Slotting as Cardano
import qualified Cardano.Chain.Update as Cardano
import qualified Cardano.Crypto as Cardano

import qualified Pos.Chain.Delegation as CSL
import qualified Pos.Chain.Genesis as CSL
import qualified Pos.Chain.Update as CSL
import qualified Pos.Core.Common as CSL
import qualified Pos.Core.Slotting as CSL
import qualified Pos.Crypto as CSL

convertHash :: CSL.AbstractHash algo a -> Cardano.AbstractHash algo b
convertHash (CSL.AbstractHash it) = Cardano.AbstractHash it

convertEpochSlots :: CSL.SlotCount -> Cardano.EpochSlots
convertEpochSlots = Cardano.EpochSlots . fromIntegral

convertRequiresNetworkMagic :: CSL.RequiresNetworkMagic -> Cardano.RequiresNetworkMagic
convertRequiresNetworkMagic rnm = case rnm of
  CSL.RequiresNoMagic -> Cardano.RequiresNoMagic
  CSL.RequiresMagic   -> Cardano.RequiresMagic

convertProtocolMagic :: CSL.ProtocolMagic -> Cardano.ProtocolMagic
convertProtocolMagic pm = Cardano.AProtocolMagic
  { Cardano.getAProtocolMagicId = Binary.Annotated (convertProtocolMagicId pmid) ()
  , Cardano.getRequiresNetworkMagic = convertRequiresNetworkMagic rnm
  }
  where
  pmid = CSL.getProtocolMagicId pm
  rnm = CSL.getRequiresNetworkMagic pm

convertProtocolMagicId :: CSL.ProtocolMagicId -> Cardano.ProtocolMagicId
convertProtocolMagicId pmid = Cardano.ProtocolMagicId (fromIntegral (CSL.unProtocolMagicId pmid))

convertAvvmDistr :: CSL.GenesisAvvmBalances -> Cardano.GenesisAvvmBalances
convertAvvmDistr = Cardano.GenesisAvvmBalances . Map.fromList . fmap convertPair . HashMap.toList . CSL.getGenesisAvvmBalances
  where
  convertPair :: (CSL.RedeemPublicKey, CSL.Coin) -> (Cardano.CompactRedeemVerificationKey, Cardano.Lovelace)
  convertPair (pubkey, coin) = (convertRedeemPublicKey pubkey, convertCoin coin)

convertRedeemPublicKey :: CSL.RedeemPublicKey -> Cardano.CompactRedeemVerificationKey
convertRedeemPublicKey (CSL.RedeemPublicKey pubkey) = Cardano.toCompactRedeemVerificationKey $ Cardano.RedeemVerificationKey pubkey

convertHeavyDelegation :: CSL.GenesisDelegation -> Cardano.GenesisDelegation
convertHeavyDelegation = Cardano.UnsafeGenesisDelegation . Map.fromList . fmap convertPair . HashMap.toList . CSL.unGenesisDelegation
  where
  convertPair :: (CSL.StakeholderId, CSL.ProxySKHeavy) -> (Cardano.KeyHash, Cardano.Certificate)
  convertPair (sid, psk) = (convertStakeholderId sid, convertProxySKHeavy psk)

convertStakeholderId :: CSL.StakeholderId -> Cardano.KeyHash
convertStakeholderId = coerce . convertAddressHash

convertAddressHash :: CSL.AddressHash algo -> Cardano.AddressHash algo
convertAddressHash (CSL.AbstractHash digest) = Cardano.AbstractHash digest

convertProxySKHeavy :: CSL.ProxySKHeavy -> Cardano.Certificate
convertProxySKHeavy psk = Cardano.UnsafeACertificate
  { Cardano.aEpoch = convertHeavyDlgIndex (CSL.pskOmega psk)
  , Cardano.issuerVK = convertPublicKey (CSL.pskIssuerPk psk)
  , Cardano.delegateVK = convertPublicKey (CSL.pskDelegatePk psk)
  , Cardano.Chain.Delegation.signature = convertCert (CSL.pskCert psk)
  , Cardano.Chain.Delegation.annotation = ()
  }

convertHeavyDlgIndex :: CSL.HeavyDlgIndex -> Binary.Annotated Cardano.EpochNumber ()
convertHeavyDlgIndex = flip Binary.Annotated () . convertEpochIndex . CSL.getHeavyDlgIndex

convertPublicKey :: CSL.PublicKey -> Cardano.VerificationKey
convertPublicKey (CSL.PublicKey pubkey) = Cardano.VerificationKey pubkey

convertCert :: CSL.ProxyCert CSL.HeavyDlgIndex -> Cardano.Signature Cardano.EpochNumber
convertCert = Cardano.Signature . CSL.unProxyCert

convertGenesisInitializer :: CSL.GenesisInitializer -> Cardano.GenesisInitializer
convertGenesisInitializer gi = Cardano.GenesisInitializer
  { Cardano.giTestBalance = convertTestnetBalanceOptions (CSL.giTestBalance gi)
  , Cardano.giFakeAvvmBalance = convertFakeAvvmOptions (CSL.giFakeAvvmBalance gi)
  , Cardano.giAvvmBalanceFactor = toRational
      (CSL.getCoinPortion (CSL.giAvvmBalanceFactor gi) % CSL.coinPortionDenominator)
  , Cardano.giUseHeavyDlg = CSL.giUseHeavyDlg gi
  }

convertTestnetBalanceOptions :: CSL.TestnetBalanceOptions -> Cardano.TestnetBalanceOptions
convertTestnetBalanceOptions tbo = Cardano.TestnetBalanceOptions
  { Cardano.tboPoors = CSL.tboPoors tbo
  , Cardano.tboRichmen = CSL.tboRichmen tbo
  , Cardano.tboTotalBalance = either (error . show) id (Cardano.mkLovelace (CSL.tboTotalBalance tbo))
  , Cardano.tboRichmenShare = toRational $ CSL.tboRichmenShare tbo
  }

convertFakeAvvmOptions :: CSL.FakeAvvmOptions -> Cardano.FakeAvvmOptions
convertFakeAvvmOptions fao = Cardano.FakeAvvmOptions
  { Cardano.faoCount = CSL.faoCount fao
  , Cardano.faoOneBalance = either (error . show) id (Cardano.mkLovelace (CSL.faoOneBalance fao))
  }

-- CSL.ScriptVersion ~ Word16
-- Cardano.ScriptVersion isn't defined, but the ProtocolConstants take
-- a Word16 for it.
convertScriptVersion :: CSL.ScriptVersion -> Word16
convertScriptVersion = id

convertSlotDuration :: Integer -> Natural
convertSlotDuration = fromIntegral

convertCoin :: CSL.Coin -> Cardano.Lovelace
convertCoin = either (error . show) id . Cardano.mkLovelace . CSL.getCoin

convertCoinPortion :: CSL.CoinPortion -> Cardano.LovelacePortion
convertCoinPortion x
  = Cardano.rationalToLovelacePortion $ toRational (CSL.getCoinPortion x % CSL.coinPortionDenominator)

-- Serokell.Data.Memory.Units.Byte -> Natural
-- I don't want to import serokell-util so we leave it open for any Enum.
convertByte :: Enum a => a -> Natural
convertByte = toEnum . fromEnum

-- CSL.FlatSlotId ~ Word64
convertFlatSlotId :: CSL.FlatSlotId -> Cardano.SlotNumber
convertFlatSlotId = Cardano.SlotNumber

convertSoftforkRule :: CSL.SoftforkRule -> Cardano.SoftforkRule
convertSoftforkRule sr = Cardano.SoftforkRule
  { Cardano.srInitThd = convertCoinPortion (CSL.srInitThd sr)
  , Cardano.srMinThd = convertCoinPortion (CSL.srMinThd sr)
  , Cardano.srThdDecrement = convertCoinPortion (CSL.srThdDecrement sr)
  }

convertTxFeePolicy :: CSL.TxFeePolicy -> Cardano.TxFeePolicy
convertTxFeePolicy txfp = case txfp of
  CSL.TxFeePolicyUnknown _ _ -> error "unknown tx fee policy not supported"
  CSL.TxFeePolicyTxSizeLinear txsl -> Cardano.TxFeePolicyTxSizeLinear
    (convertTxSizeLinear txsl)

convertTxSizeLinear :: CSL.TxSizeLinear -> Cardano.TxSizeLinear
convertTxSizeLinear (CSL.TxSizeLinear coeffA coeffB) = Cardano.TxSizeLinear
  (convertCoeff coeffA)
  (convertCoeff coeffB)

-- | In CSL.TxSizeLinear, Coeff ~ Nano is used for the coefficients in the
-- linear equation. In Cardano.TxSizeLinear, a Lovelace is used instead...
-- So, resolve the Nano to an Integer, then cast to a Word64 to make a
-- Lovelace.
convertCoeff :: CSL.Coeff -> Cardano.Lovelace
convertCoeff (CSL.Coeff nano) = either (error . show) id $ Cardano.mkLovelace $
  fromIntegral (Fixed.resolution nano)

convertEpochIndex :: CSL.EpochIndex -> Cardano.EpochNumber
convertEpochIndex = Cardano.EpochNumber . CSL.getEpochIndex

convertProtocolParameters :: CSL.BlockVersionData -> Cardano.ProtocolParameters
convertProtocolParameters bvd = Cardano.ProtocolParameters
  { Cardano.ppScriptVersion = convertScriptVersion (CSL.bvdScriptVersion bvd)
  , Cardano.ppSlotDuration = convertSlotDuration (fromIntegral (CSL.bvdSlotDuration bvd))
  , Cardano.ppMaxBlockSize = convertByte (CSL.bvdMaxBlockSize bvd)
  , Cardano.ppMaxHeaderSize = convertByte (CSL.bvdMaxHeaderSize bvd)
  , Cardano.ppMaxTxSize = convertByte (CSL.bvdMaxTxSize bvd)
  , Cardano.ppMaxProposalSize = convertByte (CSL.bvdMaxProposalSize bvd)
  , Cardano.ppMpcThd = convertCoinPortion (CSL.bvdMpcThd bvd)
  , Cardano.ppHeavyDelThd = convertCoinPortion (CSL.bvdHeavyDelThd bvd)
  , Cardano.ppUpdateVoteThd = convertCoinPortion (CSL.bvdUpdateVoteThd bvd)
  , Cardano.ppUpdateProposalThd = convertCoinPortion (CSL.bvdUpdateProposalThd bvd)
  , Cardano.ppUpdateProposalTTL = convertFlatSlotId (CSL.bvdUpdateImplicit bvd)
  , Cardano.ppSoftforkRule = convertSoftforkRule (CSL.bvdSoftforkRule bvd)
  , Cardano.ppTxFeePolicy = convertTxFeePolicy (CSL.bvdTxFeePolicy bvd)
  , Cardano.ppUnlockStakeEpoch = convertEpochIndex (CSL.bvdUnlockStakeEpoch bvd)
  }


convertGenesisSpec :: CSL.GenesisSpec -> Cardano.GenesisSpec
convertGenesisSpec gspec = Cardano.UnsafeGenesisSpec
  { Cardano.gsAvvmDistr = convertAvvmDistr (CSL.gsAvvmDistr gspec)
  , Cardano.gsHeavyDelegation = convertHeavyDelegation (CSL.gsHeavyDelegation gspec)
  , Cardano.gsProtocolParameters = convertProtocolParameters (CSL.gsBlockVersionData gspec)
  , Cardano.gsK = Cardano.BlockCount (fromIntegral (CSL.gpcK (CSL.gsProtocolConstants gspec)))
  , Cardano.gsProtocolMagic = convertProtocolMagic (CSL.gpcProtocolMagic (CSL.gsProtocolConstants gspec))
  , Cardano.gsInitializer = convertGenesisInitializer (CSL.gsInitializer gspec)
  }

-- | cardano-sl uses Timestamp ~ Microseconds for its system start time,
-- interpreted as _microsecond_ unix/posix time. cardano-ledger uses UTCTime
-- for the same purpose, so we drop it to seconds then convert.
convertSystemStart :: CSL.Timestamp -> UTCTime
convertSystemStart (CSL.Timestamp us) = posixSecondsToUTCTime secs
  where
  secs :: NominalDiffTime
  secs = fromIntegral $ (fromIntegral us :: Integer) `div` 1000000
