{-# LANGUAGE GADTSyntax #-}

module Ouroboros.Byron.Proxy.Index.Types
  ( Index (..)
  ) where

import Ouroboros.Network.Block (HeaderHash, SlotNo (..))
import Ouroboros.Network.Point (WithOrigin)
import qualified Ouroboros.Network.Point as Point (Block)

-- | An index on a single blockchain by header hash. Epoch number and relative
-- slot are indexed by the hash of the header for that epoch/slot.
--
-- It uses epoch number and relative slot, rather than SlotNo (relative
-- to genesis), because it's designed specifically to support the Byron
-- cardano-sl network interface including epoch boundary blocks. By using
-- relative slots, we can for example set slot -1 to encode the boundary block
-- for that epoch, and the index can be an injection.
data Index m header = Index
  { -- | Lookup the epoch number and relative slot for a given header hash.
    -- `Nothing` means it's not in the index.
    lookup       :: HeaderHash header -> m (Maybe SlotNo)
    -- | Check the current tip. `Nothing` means the index is empty. Otherwise,
    -- you get the point and also its header hash.
  , tip          :: m (WithOrigin (Point.Block SlotNo (HeaderHash header)))
    -- | Extend the index with a new entry. The point must be newer than
    -- the latest point in the index (current tip). Whether this is checked
    -- or enforced depends upon the implementation.
  , rollforward  :: header -> m ()
    -- | Roll back to a given point, making it the tip of the index.
    -- TPoint is used because you can rollback to the origin, clearing the
    -- index.
    -- An index implementation need not actually use the header hash here.
    -- It could or could not check that the point actually corresponds to the
    -- entry at that hash.
  , rollbackward :: WithOrigin (Point.Block SlotNo (HeaderHash header)) -> m ()
  }
