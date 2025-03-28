module CardanoLightning.Transaction.Validator where

import qualified Cardano.Api as C
import qualified Cardano.Api.Ledger as CL
import Cardano.Binary (FromCBOR, ToCBOR (toCBOR), fromCBOR)
import CardanoLightning.Contrib.Cardano.Api (RawBytes (..))
import CardanoLightning.Contrib.Data.Aeson (HexEncoded (..))
import Codec.Serialise (Serialise (decode, encode))
import Control.Category ((>>>))
import Data.Aeson (FromJSON (parseJSON), ToJSON, object, toJSON, withArray, withObject, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import Data.Data (Typeable)
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import qualified PlutusCore.Data as PC
import qualified PlutusLedgerApi.Common as P
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

newtype Signature = Signature ByteString
    deriving (Eq, Show)

deriving via
    HexEncoded
    instance
        ToJSON Signature

deriving via
    HexEncoded
    instance
        FromJSON Signature

deriving via
    (RawBytes ByteString)
    instance
        P.ToData Signature

deriving via
    (RawBytes ByteString)
    instance
        P.FromData Signature

newtype VerificationKey = VerificationKey (C.VerificationKey C.PaymentKey)
    deriving (Eq, Show)

deriving via
    RawBytes (C.VerificationKey C.PaymentKey)
    instance
        ToJSON VerificationKey

deriving via
    RawBytes (C.VerificationKey C.PaymentKey)
    instance
        FromJSON VerificationKey

deriving via
    RawBytes (C.VerificationKey C.PaymentKey)
    instance
        P.FromData VerificationKey

deriving via
    RawBytes (C.VerificationKey C.PaymentKey)
    instance
        P.ToData VerificationKey

data Keys = Keys VerificationKey VerificationKey
    deriving (Eq, Show)

instance FromJSON Keys where
    parseJSON = withArray "Keys" $ \v -> do
        [vk1, vk2] <- pure $ Vector.toList v
        Keys <$> parseJSON vk1 <*> parseJSON vk2

instance ToJSON Keys where
    toJSON (Keys vk1 vk2) =
        toJSON [toJSON vk1, toJSON vk2]

instance P.ToData Keys where
    toBuiltinData (Keys vk1 vk2) = do
        P.toBuiltinData [P.toBuiltinData vk1, P.toBuiltinData vk2]

instance P.FromData Keys where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [vk1, vk2] ->
            Keys
                <$> P.fromBuiltinData (P.dataToBuiltinData vk1)
                <*> P.fromBuiltinData (P.dataToBuiltinData vk2)
        _ -> P.Nothing

data Signed a = Signed a Signature
    deriving (Eq, Show)

instance (FromJSON a) => FromJSON (Signed a) where
    parseJSON = withArray "Signed" $ \v -> do
        [value, signature] <- pure $ Vector.toList v
        Signed
            <$> parseJSON value
            <*> parseJSON signature

instance (ToJSON a) => ToJSON (Signed a) where
    toJSON (Signed value signature) =
        toJSON [toJSON value, toJSON signature]

instance (P.ToData a) => P.ToData (Signed a) where
    toBuiltinData (Signed value signature) =
        P.toBuiltinData [P.toBuiltinData value, P.toBuiltinData signature]

type Receipt = (Maybe (Signed Snapshot), [Signed Cheque])

newtype PlutusBasedCBOR a = PlutusBasedCBOR a

instance (P.ToData a, Typeable a) => ToCBOR (PlutusBasedCBOR a) where
    toCBOR (PlutusBasedCBOR a) = do
        encode . P.toData $ a

instance (P.FromData a, Typeable a) => FromCBOR (PlutusBasedCBOR a) where
    fromCBOR = do
        plutusData <- decode
        case P.fromBuiltinData $ P.dataToBuiltinData plutusData of
            Just a -> pure $ PlutusBasedCBOR a
            Nothing -> fail "Failed to decode Plutus data"

data HtlcSecret
    = Sha2_256Secret ByteString
    | Sha3_256Secret ByteString
    | Blake2b_256Secret ByteString
    deriving (Eq, Show)

instance FromJSON HtlcSecret where
    parseJSON = withObject "HtlcSecret" $ \o -> do
        constructor <- o .: "constructor" :: A.Parser Text
        (getHexEncoded -> bytes) <- o .: "inputs"
        case constructor of
            "Sha2_256Secret" -> pure $ Sha2_256Secret bytes
            "Sha3_256Secret" -> pure $ Sha3_256Secret bytes
            "Blake2b_256Secret" -> pure $ Blake2b_256Secret bytes
            _ -> fail "Unknown HtlcSecret constructor"

instance ToJSON HtlcSecret where
    toJSON (Sha2_256Secret bs) =
        object
            [ "constructor" .= ("Sha2_256Secret" :: Text)
            , "inputs" .= HexEncoded bs
            ]
    toJSON (Sha3_256Secret bs) =
        object
            [ "constructor" .= ("Sha3_256Secret" :: Text)
            , "inputs" .= HexEncoded bs
            ]
    toJSON (Blake2b_256Secret bs) =
        object
            [ "constructor" .= ("Blake2b_256Secret" :: Text)
            , "inputs" .= HexEncoded bs
            ]

instance P.ToData HtlcSecret where
    toBuiltinData (Sha2_256Secret bs) = P.mkConstr 0 [P.toBuiltinData $ RawBytes bs]
    toBuiltinData (Sha3_256Secret bs) = P.mkConstr 1 [P.toBuiltinData $ RawBytes bs]
    toBuiltinData (Blake2b_256Secret bs) = P.mkConstr 2 [P.toBuiltinData $ RawBytes bs]

type Secrets = [(Index, HtlcSecret)]

data Cheque
    = Normal Index Amount
    | Htlc Index Timeout HtlcLock Amount
    | HtlcUnlocked Index Timeout HtlcSecret Amount
    deriving (Eq, Show)

instance FromJSON Cheque where
    parseJSON = withObject "Cheque" $ \o -> do
        constructor <- o .: "constructor" :: A.Parser Text
        inputs <- o .: "inputs"
        case constructor of
            "Normal" -> do
                [index, amount] <- pure $ Vector.toList inputs
                Normal <$> parseJSON index <*> parseJSON amount
            "Htlc" -> do
                [index, timeout, lock, amount] <- pure $ Vector.toList inputs
                Htlc
                    <$> parseJSON index
                    <*> parseJSON timeout
                    <*> parseJSON lock
                    <*> parseJSON amount
            "HtlcUnlocked" -> do
                [index, timeout, secret, amount] <- pure $ Vector.toList inputs
                HtlcUnlocked
                    <$> parseJSON index
                    <*> parseJSON timeout
                    <*> parseJSON secret
                    <*> parseJSON amount
            _ -> fail "Unknown Cheque constructor"

instance ToJSON Cheque where
    toJSON (Normal idx amt) =
        object
            [ "constructor" .= ("Normal" :: Text)
            , "inputs" .= toJSON [toJSON idx, toJSON amt]
            ]
    toJSON (Htlc idx timeout lock amt) =
        object
            [ "constructor" .= ("Htlc" :: Text)
            , "inputs" .= toJSON [toJSON idx, toJSON timeout, toJSON lock, toJSON amt]
            ]
    toJSON (HtlcUnlocked idx timeout secret amt) =
        object
            [ "constructor" .= ("HtlcUnlocked" :: Text)
            , "inputs" .= toJSON [toJSON idx, toJSON timeout, toJSON secret, toJSON amt]
            ]

instance P.ToData Cheque where
    toBuiltinData (Normal idx amt) =
        P.mkConstr 0 [P.toBuiltinData idx, P.toBuiltinData amt]
    toBuiltinData (Htlc idx timeout lock amt) =
        P.mkConstr 1 [P.toBuiltinData idx, P.toBuiltinData timeout, P.toBuiltinData lock, P.toBuiltinData amt]
    toBuiltinData (HtlcUnlocked idx timeout secret amt) =
        P.mkConstr 2 [P.toBuiltinData idx, P.toBuiltinData timeout, P.toBuiltinData secret, P.toBuiltinData amt]

newtype Amount = Amount Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)
instance Semigroup Amount where
    Amount a <> Amount b = Amount (a + b)
instance Monoid Amount where
    mempty = Amount 0

newtype Index = Index Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

data Squash = Squash
    { amount :: Amount
    , exclude :: [Index]
    , index :: Index
    }
    deriving (Eq, Ord, Show)

emptySquash :: Squash
emptySquash = Squash (Amount 0) [] (Index 0)

instance P.ToData Squash where
    toBuiltinData (Squash{amount, index, exclude}) =
        P.toBuiltinData
            [P.toBuiltinData amount, P.toBuiltinData index, P.toBuiltinData exclude]

instance P.FromData Squash where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [amount, index, exclude] ->
            Squash
                <$> P.fromBuiltinData (P.dataToBuiltinData amount)
                <*> P.fromBuiltinData (P.dataToBuiltinData exclude)
                <*> P.fromBuiltinData (P.dataToBuiltinData index)
        _ -> P.Nothing

instance FromJSON Squash where
    parseJSON v =
        flip (withArray ("Squash:" <> show v)) v $
            Vector.toList >>> \case
                [amountJson, indexJson, excludeJson] ->
                    Squash
                        <$> parseJSON amountJson
                        <*> parseJSON excludeJson
                        <*> parseJSON indexJson
                _ -> fail "Expected array of 3 elements"

instance ToJSON Squash where
    toJSON (Squash{amount, index, exclude}) =
        toJSON [toJSON amount, toJSON index, toJSON exclude]

data Snapshot = Snapshot Squash Squash
    deriving (Eq, Ord, Show)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot emptySquash emptySquash

instance FromJSON Snapshot where
    parseJSON v =
        flip (withArray ("Snapshot:" <> show v)) v $
            Vector.toList >>> \case
                [squash1, squash2] -> Snapshot <$> parseJSON squash1 <*> parseJSON squash2
                _ -> fail "Expected array of 2 elements"

instance ToJSON Snapshot where
    toJSON (Snapshot squash1 squash2) =
        toJSON [squash1, squash2]

instance P.ToData Snapshot where
    toBuiltinData (Snapshot a b) = P.toBuiltinData [P.toBuiltinData a, P.toBuiltinData b]

instance P.FromData Snapshot where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [a, b] ->
            Snapshot
                <$> P.fromBuiltinData (P.dataToBuiltinData a)
                <*> P.fromBuiltinData (P.dataToBuiltinData b)
        _ -> P.Nothing

newtype POSIXTimeMilliseconds = POSIXTimeMilliseconds Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

newtype DiffTimeMilliseconds = DiffTimeMilliseconds Integer
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

newtype Period = Period DiffTimeMilliseconds
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

newtype Timeout = Timeout POSIXTimeMilliseconds
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

newtype HtlcLock = HtlcLock ByteString
    deriving (Eq, Ord, Show)

deriving via
    HexEncoded
    instance
        FromJSON HtlcLock

deriving via
    HexEncoded
    instance
        ToJSON HtlcLock

deriving via
    RawBytes ByteString
    instance
        P.ToData HtlcLock

deriving via
    RawBytes ByteString
    instance
        P.FromData HtlcLock

newtype Pend = Pend [HtlcLockReduced]
    deriving (Eq, Ord, Show)
    deriving newtype (P.ToData, P.FromData, FromJSON, ToJSON)

data HtlcLockReduced = HtlcLockReduced Amount Timeout HtlcLock
    deriving (Eq, Ord, Show)

instance P.FromData HtlcLockReduced where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [amount, timeout, htlcLock] ->
            HtlcLockReduced
                <$> P.fromBuiltinData (P.dataToBuiltinData amount)
                <*> P.fromBuiltinData (P.dataToBuiltinData timeout)
                <*> P.fromBuiltinData (P.dataToBuiltinData htlcLock)
        _ -> P.Nothing

instance P.ToData HtlcLockReduced where
    toBuiltinData (HtlcLockReduced amount timeout htlcLock) =
        P.toBuiltinData
            [P.toBuiltinData amount, P.toBuiltinData timeout, P.toBuiltinData htlcLock]

instance FromJSON HtlcLockReduced where
    parseJSON = withObject "HtlcLockReduced" \o -> do
        amount <- o .: "amount"
        timeout <- o .: "timeout"
        htlcLock <- o .: "htlcLock"
        return $ HtlcLockReduced amount timeout htlcLock

instance ToJSON HtlcLockReduced where
    toJSON (HtlcLockReduced amount timeout htlcLock) =
        object
            [ "amount" .= amount
            , "timeout" .= timeout
            , "htlcLock" .= htlcLock
            ]

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
            [ P.toBuiltinData amount
            , P.toBuiltinData snapshot
            , P.toBuiltinData period
            ]
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

instance P.FromData Stage where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [PC.I 0, amount, snapshot, period] ->
            Opened
                <$> P.fromBuiltinData (P.dataToBuiltinData amount)
                <*> P.fromBuiltinData (P.dataToBuiltinData snapshot)
                <*> P.fromBuiltinData (P.dataToBuiltinData period)
        PC.List [PC.I 1, amount, squash, timeout, pend] ->
            Closed
                <$> P.fromBuiltinData (P.dataToBuiltinData amount)
                <*> P.fromBuiltinData (P.dataToBuiltinData squash)
                <*> P.fromBuiltinData (P.dataToBuiltinData timeout)
                <*> P.fromBuiltinData (P.dataToBuiltinData pend)
        PC.List [PC.I 2, amount, pend1, pend2] ->
            Responded
                <$> P.fromBuiltinData (P.dataToBuiltinData amount)
                <*> P.fromBuiltinData (P.dataToBuiltinData pend1)
                <*> P.fromBuiltinData (P.dataToBuiltinData pend2)
        PC.List [PC.I 3, pend1, pend2] ->
            Resolved
                <$> P.fromBuiltinData (P.dataToBuiltinData pend1)
                <*> P.fromBuiltinData (P.dataToBuiltinData pend2)
        PC.List [PC.I 4, pend] ->
            Elapsed
                <$> P.fromBuiltinData (P.dataToBuiltinData pend)
        _ -> P.Nothing

instance FromJSON Stage where
    parseJSON = withObject "Stage" \o -> do
        (constructor :: Text) <- o .: "constructor"
        inputs <- o .: "inputs"
        case constructor of
            -- = Opened Amount Snapshot Period
            "Opened" ->
                Opened
                    <$> inputs .: "amount"
                    <*> inputs .: "snapshot"
                    <*> inputs .: "period"
            "Closed" ->
                Closed
                    <$> inputs .: "amount"
                    <*> inputs .: "squash"
                    <*> inputs .: "timeout"
                    <*> inputs .: "pend"
            "Responded" ->
                Responded
                    <$> inputs .: "amount"
                    <*> inputs .: "pend1"
                    <*> inputs .: "pend2"
            "Resolved" ->
                Resolved
                    <$> inputs .: "pend1"
                    <*> inputs .: "pend2"
            "Elapsed" ->
                Elapsed
                    <$> inputs .: "pend"
            _ -> fail "Unknown constructor"

instance ToJSON Stage where
    toJSON (Opened amount snapshot period) =
        object
            [ "constructor" .= ("Opened" :: Text)
            , "inputs"
                .= object
                    [ "amount" .= amount
                    , "snapshot" .= snapshot
                    , "period" .= period
                    ]
            ]
    toJSON (Closed amount squash timeout pend) =
        object
            [ "constructor" .= ("Closed" :: Text)
            , "inputs"
                .= object
                    [ "amount" .= amount
                    , "squash" .= squash
                    , "timeout" .= timeout
                    , "pend" .= pend
                    ]
            ]
    toJSON (Responded amount pend1 pend2) =
        object
            [ "constructor" .= ("Responded" :: Text)
            , "inputs"
                .= object
                    [ "amount" .= amount
                    , "pend1" .= pend1
                    , "pend2" .= pend2
                    ]
            ]
    toJSON (Resolved pend1 pend2) =
        object
            [ "constructor" .= ("Resolved" :: Text)
            , "inputs"
                .= object
                    [ "pend1" .= pend1
                    , "pend2" .= pend2
                    ]
            ]
    toJSON (Elapsed pend) =
        object
            [ "constructor" .= ("Elapsed" :: Text)
            , "inputs"
                .= object
                    [ "pend" .= pend
                    ]
            ]

deriving via
    (PlutusBasedCBOR Stage)
    instance
        ToCBOR Stage

data Datum = Datum
    { scriptHash :: C.ScriptHash
    , keys :: Keys
    , stage :: Stage
    }
    deriving (Eq, Show)

instance FromJSON Datum where
    parseJSON = withArray "Datum" $ \v -> do
        [hash, keys, stage] <- pure $ Vector.toList v
        Datum
            <$> parseJSON hash
            <*> parseJSON keys
            <*> parseJSON stage

instance ToJSON Datum where
    toJSON (Datum hash keys stage) =
        toJSON [toJSON hash, toJSON keys, toJSON stage]

instance P.FromData Datum where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [hash, keys, stage] -> do
            RawBytes hashBytes <- P.fromBuiltinData (P.dataToBuiltinData hash)
            Datum hashBytes
                <$> P.fromBuiltinData (P.dataToBuiltinData keys)
                <*> P.fromBuiltinData (P.dataToBuiltinData stage)
        _ -> P.Nothing

instance P.ToData Datum where
    toBuiltinData (Datum hash keys stage) =
        P.toBuiltinData [P.toBuiltinData $ RawBytes hash, P.toBuiltinData keys, P.toBuiltinData stage]

data SpendRedeemer = DeferToMint
    deriving (Eq, Show)

instance FromJSON SpendRedeemer where
    parseJSON = withObject "SpendRedeemer" $ \o -> do
        (constructor :: Text) <- o .: "constructor"
        case constructor of
            "DeferToMint" -> pure DeferToMint
            _ -> fail "Unknown SpendRedeemer constructor"

instance ToJSON SpendRedeemer where
    toJSON DeferToMint = object ["constructor" .= ("DeferToMint" :: Text)]

instance P.ToData SpendRedeemer where
    toBuiltinData DeferToMint = P.mkConstr 0 []

-- Aiken:
-- pub type OutputReference {
--   transaction_id: Hash<Blake2b_256, Transaction>,
--   output_index: Int,
-- }

newtype MintingSeed = MintingSeed C.TxIn
    deriving (Eq, Show)

instance FromJSON MintingSeed where
    parseJSON = withObject "MintingSeed" $ \o -> do
        txIn <- o .: "txIn"
        pure $ MintingSeed txIn

instance ToJSON MintingSeed where
    toJSON (MintingSeed txIn) =
        object ["txIn" .= toJSON txIn]

instance P.ToData MintingSeed where
    toBuiltinData (MintingSeed (C.TxIn txId (C.TxIx txIx))) =
        P.mkConstr
            0
            [ P.toBuiltinData $ RawBytes txId
            , P.toBuiltinData $ toInteger txIx
            ]

instance P.FromData MintingSeed where
    fromBuiltinData d = case P.builtinDataToData d of
        PC.List [PC.I 0, P.B txId, P.I txIx] -> do
            txId' <- CL.hashFromBytes txId
            pure $ MintingSeed (C.TxIn (C.TxId txId') (C.TxIx $ fromInteger txIx))
        _ -> P.Nothing

data MintRedeemer = MintRedeemer
    { seed :: Maybe MintingSeed
    , steps :: [NStep]
    }
    deriving (Eq, Show)

instance ToJSON MintRedeemer where
    toJSON (MintRedeemer seed steps) =
        toJSON [toJSON seed, toJSON steps]

instance FromJSON MintRedeemer where
    parseJSON = withArray "MintRedeemer" $ \v -> do
        [seed, steps] <- pure $ Vector.toList v
        MintRedeemer
            <$> parseJSON seed
            <*> parseJSON steps

instance P.ToData MintRedeemer where
    toBuiltinData (MintRedeemer seed steps) =
        P.toBuiltinData [P.toBuiltinData seed, P.toBuiltinData steps]

data NStep
    = Continuing CStep
    | End Secrets
    deriving (Eq, Show)

instance FromJSON NStep where
    parseJSON = withObject "NStep" $ \o -> do
        constructor <- o .: "constructor"
        inputs <- o .: "inputs"
        case constructor of
            ("Continuing" :: Text) -> Continuing <$> parseJSON inputs
            "End" -> End <$> parseJSON inputs
            _ -> fail "Unknown NStep constructor"

instance ToJSON NStep where
    toJSON (Continuing step) =
        object
            [ "constructor" .= ("Continuing" :: Text)
            , "inputs" .= toJSON step
            ]
    toJSON (End secrets) =
        object
            [ "constructor" .= ("End" :: Text)
            , "inputs" .= toJSON secrets
            ]

instance P.ToData NStep where
    toBuiltinData (Continuing step) = P.mkConstr 0 [P.toBuiltinData step]
    toBuiltinData (End secrets) = P.mkConstr 1 [P.toBuiltinData secrets]

data CStep
    = Add (Maybe (Signed Snapshot))
    | Close Receipt
    | Respond Receipt Bool
    | Resolve Secrets
    | Elapse Secrets
    | Free Secrets Bool
    deriving (Eq, Show)

instance FromJSON CStep where
    parseJSON = withObject "CStep" $ \o -> do
        (constructor :: Text) <- o .: "constructor"
        inputs@(A.Object o') <- o .: "inputs"
        case constructor of
            "Add" -> Add <$> parseJSON inputs
            "Close" -> Close <$> parseJSON inputs
            "Respond" ->
                Respond
                    <$> (o' .: "receipt")
                    <*> (o' .: "dropOld")
            "Resolve" -> Resolve <$> parseJSON inputs
            "Elapse" -> Elapse <$> parseJSON inputs
            "Free" ->
                Free
                    <$> (o' .: "secrets")
                    <*> (o' .: "dropOld")
            _ -> fail "Unknown CStep constructor"

instance ToJSON CStep where
    toJSON (Add snapshot) =
        object
            [ "constructor" .= ("Add" :: Text)
            , "inputs" .= toJSON snapshot
            ]
    toJSON (Close receipt) =
        object
            [ "constructor" .= ("Close" :: Text)
            , "inputs" .= toJSON receipt
            ]
    toJSON (Respond receipt dropOld) =
        object
            [ "constructor" .= ("Respond" :: Text)
            , "inputs"
                .= object
                    [ "receipt" .= receipt
                    , "dropOld" .= dropOld
                    ]
            ]
    toJSON (Resolve secrets) =
        object
            [ "constructor" .= ("Resolve" :: Text)
            , "inputs" .= toJSON secrets
            ]
    toJSON (Elapse secrets) =
        object
            [ "constructor" .= ("Elapse" :: Text)
            , "inputs" .= toJSON secrets
            ]
    toJSON (Free secrets dropOld) =
        object
            [ "constructor" .= ("Free" :: Text)
            , "inputs"
                .= object
                    [ "secrets" .= secrets
                    , "dropOld" .= dropOld
                    ]
            ]

instance P.ToData CStep where
    toBuiltinData (Add snapshot) = P.mkConstr 0 [P.toBuiltinData snapshot]
    toBuiltinData (Close receipt) = P.mkConstr 1 [P.toBuiltinData receipt]
    toBuiltinData (Respond receipt dropOld) = P.mkConstr 2 [P.toBuiltinData receipt, P.toBuiltinData dropOld]
    toBuiltinData (Resolve secrets) = P.mkConstr 3 [P.toBuiltinData secrets]
    toBuiltinData (Elapse secrets) = P.mkConstr 4 [P.toBuiltinData secrets]
    toBuiltinData (Free secrets dropOld) = P.mkConstr 5 [P.toBuiltinData secrets, P.toBuiltinData dropOld]

newtype ChannelId = ChannelId ByteString

deriving via
    RawBytes ByteString
    instance
        P.ToData ChannelId

deriving via
    RawBytes ByteString
    instance
        P.FromData ChannelId

deriving via
    HexEncoded
    instance
        ToJSON ChannelId

deriving via
    HexEncoded
    instance
        FromJSON ChannelId

newtype OpeningIx = OpeningIx Word8

mkChannelId :: MintingSeed -> OpeningIx -> ChannelId
mkChannelId mintingSeed (OpeningIx ix) = do
    let
        message =
            P.consByteString (fromIntegral ix) $
                P.serialiseData $
                    P.toBuiltinData mintingSeed
        hash = P.blake2b_256 message
    ChannelId $ P.fromBuiltin hash
