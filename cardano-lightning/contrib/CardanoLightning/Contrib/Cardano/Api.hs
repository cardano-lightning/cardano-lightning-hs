module CardanoLightning.Contrib.Cardano.Api (
    AUTxO (..),
    CBORBytes (..),
    RawBytes (..),
    foldToUTxO,
    toUTxO,
) where

import Cardano.Api (
    SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR),
    SerialiseAsRawBytes (..),
    deserialiseFromRawBytes,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (HasTypeProxy (..))
import CardanoLightning.Contrib.Data.Aeson (HexEncoded (..))
import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON), (.=))
import Data.ByteString (ByteString)
import Data.Data (Proxy (..))
import qualified Data.List as L
import qualified Data.Map as M
import qualified PlutusTx as P
import qualified PlutusTx.Prelude as P

data AUTxO era = AUTxO {aTxIn :: C.TxIn, aTxOut :: C.TxOut C.CtxUTxO era}
    deriving (Show)

instance (C.IsCardanoEra era) => ToJSON (AUTxO era) where
    toJSON (AUTxO txIn txOut) =
        A.object
            [ "txIn" .= txIn
            , "txOut" .= txOut
            ]

instance (C.IsCardanoEra era, C.IsShelleyBasedEra era) => FromJSON (AUTxO era) where
    parseJSON = A.withObject "AUTxO" $ \v -> do
        txIn <- v .: "txIn"
        txOut <- v .: "txOut"
        pure $ AUTxO txIn txOut

toUTxO :: AUTxO era -> C.UTxO era
toUTxO (AUTxO txin txout) = C.UTxO . M.fromList $ [(txin, txout)]

foldToUTxO :: (Foldable f) => f (AUTxO era) -> C.UTxO era
foldToUTxO =
    C.UTxO
        . foldMap (M.fromList . L.singleton . \(AUTxO txin txout) -> (txin, txout))

newtype RawBytes a = RawBytes {getRawBytesValue :: a}

instance {-# OVERLAPPING #-} ToJSON (RawBytes ByteString) where
    toJSON = toJSON . HexEncoded . getRawBytesValue

instance {-# OVERLAPPING #-} FromJSON (RawBytes ByteString) where
    parseJSON json = do
        HexEncoded bytes <- parseJSON json
        pure $ RawBytes bytes

instance (SerialiseAsRawBytes a) => ToJSON (RawBytes a) where
    toJSON = toJSON . HexEncoded . serialiseToRawBytes . getRawBytesValue

instance (SerialiseAsRawBytes a) => FromJSON (RawBytes a) where
    parseJSON json = do
        HexEncoded bytes <- parseJSON json
        case deserialiseFromRawBytes (proxyToAsType $ Proxy @a) bytes of
            Left err -> fail $ show err
            Right a -> pure $ RawBytes a

instance {-# OVERLAPPING #-} P.ToData (RawBytes ByteString) where
    toBuiltinData = P.toBuiltinData . P.toBuiltin . getRawBytesValue

instance {-# OVERLAPPING #-} P.FromData (RawBytes ByteString) where
    fromBuiltinData plutusData = do
        (builtinbytes :: P.BuiltinByteString) <- P.fromBuiltinData plutusData
        pure $ RawBytes $ P.fromBuiltin builtinbytes

instance (SerialiseAsRawBytes a) => P.ToData (RawBytes a) where
    toBuiltinData = P.toBuiltinData . P.toBuiltin . serialiseToRawBytes . getRawBytesValue

instance (SerialiseAsRawBytes a) => P.FromData (RawBytes a) where
    fromBuiltinData plutusData = do
        (builtinbytes :: P.BuiltinByteString) <- P.fromBuiltinData plutusData
        let
            bytes = P.fromBuiltin builtinbytes
        case deserialiseFromRawBytes (proxyToAsType $ Proxy @a) bytes of
            Left err -> fail $ show err
            Right a -> pure $ RawBytes a

newtype CBORBytes a = CBORBytes {getCBORBytes :: a}

instance (SerialiseAsCBOR a) => ToJSON (CBORBytes a) where
    toJSON = toJSON . HexEncoded . serialiseToCBOR . getCBORBytes

instance (SerialiseAsCBOR a) => FromJSON (CBORBytes a) where
    parseJSON json = do
        HexEncoded bytes <- parseJSON json
        case deserialiseFromCBOR (proxyToAsType $ Proxy @a) bytes of
            Left err -> fail $ show err
            Right a -> pure $ CBORBytes a
