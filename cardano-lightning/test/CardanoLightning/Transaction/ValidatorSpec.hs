{-# OPTIONS_GHC -Wno-unused-imports #-}

module CardanoLightning.Transaction.ValidatorSpec where

import Cardano.Binary (toCBOR)
import qualified Cardano.Binary as CBOR
import CardanoLightning.Contrib.Data.Aeson (HexEncoded (..))
import CardanoLightning.Transaction.Validator (Stage (..))
import Data.Aeson (FromJSON, ToJSON (toJSON), parseJSON, withObject, (.:))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Debug.Trace (traceM)
import Test.Hspec as H
import Test.QuickCheck as Q

-- Stage data type:
-- data Stage
--     = Opened Amount Snapshot Period
--     | Closed Amount Squash Timeout Pend
--     | Responded Amount Pend Pend
--     | Resolved Pend Pend
--     | Elapsed Pend
--     deriving (Eq, Ord, Show)
--
-- Example JSON:
--   {
--     "constructor": "Opened",
--     "inputs": {
--       "amount": 480799053325262,
--       "snapshot": [
--         [
--           9435004930622,
--           185864762,
--           [
--             401540620,
--             528266558,
--             435255674,
--             640323458,
--             886502555,
--             281745993,
--             889103970,
--             308000441,
--             657906575
--           ]
--         ],
--         [
--           737735303720198,
--           705832487,
--           [
--             882868342,
--             719798894,
--             309293734,
--             537678804,
--             335596553,
--             335975489,
--             852599752,
--             392038008
--           ]
--         ]
--       ],
--       "period": 7785
--     },
--     "cbor": "d8799f1b0001b548c0c823ce9f9f1b00000894c21e0a3e1a0b14123a9f1a17ef060c1a1f7cb53e1a19f1797a1a262a8f821a34d6f49b1a10cb1a491a34fea6621a125bb6b91a2736db8fffff9f1b00029ef762b76d061a2a1226279f1a349f80761a2ae7426e1a126f72a61a200c53d41a1400cc091a140694411a32d1a3c81a175e0678ffffff191e69ff"
--   },
--

-- | Represents the structure of our test vectors
data StageTestVector = StageTestVector
    { cbor :: ByteString
    , stage :: Stage
    }
    deriving (Eq, Show)

instance FromJSON StageTestVector where
    parseJSON json = do
        traceM $ show json
        stage <- parseJSON json
        cbor <- flip (withObject "StageTestVector") json $ \o -> do
            HexEncoded cborHex <- o .: "cbor"
            pure cborHex
        pure $ StageTestVector cbor stage

-- -- Property test helpers

-- prop_stageSerializationMatchesTestVector vector =
--     let stage = testVectorToStage vector
--         expected = cbor vector
--      in -- actual = serializeStage stage -- You'll need to implement this
--         counterexample
--             ("Expected: " ++ expected ++ "\nActual: " {- ++ actual -})
--             True -- (expected == actual)
--
expectRight :: (Show a) => Either a b -> IO b
expectRight (Right x) = pure x
expectRight (Left e) = fail $ "Expected Right, got Left: " ++ show e

spec :: Spec
spec = describe "Stage Serialization" $ do
    it "should serialize test vector values correctly" $ do
        json <- B.readFile "./cardano-lightning/test/Fixtures/stage.json"
        (stageVectors :: [StageTestVector]) <- expectRight . A.eitherDecode . B.fromStrict $ json
        for_ stageVectors \testVector -> do
            let StageTestVector{..} = testVector
                actual = B.toStrict $ CBOR.serialize $ toCBOR stage
            -- traceM $ A.encodeUtf8 $ toJSON $ HexEncoded actual
            actual `shouldBe` cbor

-- pending "Implement test vector validation"
