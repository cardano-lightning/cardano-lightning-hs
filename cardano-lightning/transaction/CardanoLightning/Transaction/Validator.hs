module CardanoLightning.Transaction.Validator where

-- import qualified Cardano.Api as C

import Data.ByteString (ByteString)
import qualified PlutusLedgerApi.Common as P
import qualified PlutusTx.Builtins as P

newtype Amount = Amount Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)
instance Semigroup Amount where
    Amount a <> Amount b = Amount (a + b)
instance Monoid Amount where
    mempty = Amount 0

newtype Index = Index Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

data Squash = Squash
    { amount :: Amount
    , exclude :: [Index]
    , index :: Index
    }
    deriving (Eq, Ord, Show)

instance P.ToData Squash where
    toBuiltinData (Squash{amount, index, exclude}) =
        P.toBuiltinData
            [P.toBuiltinData amount, P.toBuiltinData index, P.toBuiltinData exclude]

data Snapshot = Snapshot Squash Squash
    deriving (Eq, Ord, Show)

instance P.ToData Snapshot where
    toBuiltinData (Snapshot a b) = P.toBuiltinData [P.toBuiltinData a, P.toBuiltinData b]

newtype POSIXTimeMilliseconds = POSIXTimeMilliseconds Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

newtype DiffTimeMilliseconds = DiffTimeMilliseconds Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

newtype Period = Period DiffTimeMilliseconds
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

newtype Timeout = Timeout POSIXTimeMilliseconds
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

newtype HtlcLock = HtlcLock ByteString
    deriving (Eq, Ord, Show)

instance P.ToData HtlcLock where
    toBuiltinData (HtlcLock bs) = P.mkB . P.toBuiltin $ bs

newtype Pend = Pend [HtlcLockReduced]
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData)

data HtlcLockReduced = HtlcLockReduced Amount Timeout HtlcLock
    deriving (Eq, Ord, Show)

instance P.ToData HtlcLockReduced where
    toBuiltinData (HtlcLockReduced amount timeout htlcLock) =
        P.toBuiltinData
            [P.toBuiltinData amount, P.toBuiltinData timeout, P.toBuiltinData htlcLock]

-- Aiken:
-- pub type Stage {
--   Opened(Amount, Snapshot, Period)
--   Closed(Amount, Squash, Timeout, Pend)
--   Responded(Amount, Pend, Pend)
--   Resolved(Pend, Pend)
--   Elapsed(Pend)
-- }

data Stage
    = Opened Amount Snapshot Period
    | Closed Amount Squash Timeout Pend
    | Responded Amount Pend Pend
    | Resolved Pend Pend
    | Elapsed Pend
    deriving (Eq, Ord, Show)

instance P.ToData Stage where
    toBuiltinData (Opened amount snapshot period) =
        P.mkConstr
            0
            [P.toBuiltinData amount, P.toBuiltinData snapshot, P.toBuiltinData period]
    toBuiltinData (Closed amount squash timeout pend) =
        P.mkConstr
            1
            [P.toBuiltinData amount, P.toBuiltinData squash, P.toBuiltinData timeout, P.toBuiltinData pend]
    toBuiltinData (Responded amount pend1 pend2) =
        P.mkConstr
            2
            [P.toBuiltinData amount, P.toBuiltinData pend1, P.toBuiltinData pend2]
    toBuiltinData (Resolved pend1 pend2) =
        P.mkConstr
            3
            [P.toBuiltinData pend1, P.toBuiltinData pend2]
    toBuiltinData (Elapsed pend) =
        P.mkConstr
            4
            [P.toBuiltinData pend]
