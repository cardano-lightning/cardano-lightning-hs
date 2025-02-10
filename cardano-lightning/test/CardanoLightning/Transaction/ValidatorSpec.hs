module CardanoLightning.Transaction.ValidatorSpec where

import CardanoLightning.Transaction.Validator
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Hex as Hex
import Test.Hspec
import Test.QuickCheck

-- | Represents the structure of our test vectors
data StageTestVector = StageTestVector
    { stage :: Stage
    , cbor :: String
    }
    deriving (Eq, Show)

-- | Represents the different possible inputs based on Stage constructor
data StageInputs = OpenedInputs
    { amount :: Integer
    , snapshot :: ([[Integer]], [[Integer]]) -- ((amount,index,[exclude]), (amount,index,[exclude]))
    , period :: Integer
    }
    deriving (Eq, Show)

-- JSON instances
instance FromJSON StageTestVector where
    parseJSON = withObject "StageTestVector" $ \v ->
        StageTestVector
            <$> v .: "constructor"
            <*> v .: "inputs"
            <*> v .: "cbor"

instance FromJSON StageInputs where
    parseJSON = withObject "StageInputs" $ \v -> do
        constructor <- v .: "constructor"
        case constructor of
            "Opened" ->
                OpenedInputs
                    <$> v .: "amount"
                    <*> v .: "snapshot"
                    <*> v .: "period"
            _ -> fail $ "Unsupported constructor: " ++ constructor

-- | Convert test vector into actual Stage type
testVectorToStage :: StageTestVector -> Stage
testVectorToStage StageTestVector{constructor, inputs} =
    case (constructor, inputs) of
        ("Opened", OpenedInputs{amount, snapshot, period}) ->
            let (squash1, squash2) = snapshot
                makeSquash [amt, idx, excl] =
                    Squash
                        { amount = Amount amt
                        , index = Index idx
                        , exclude = map Index excl
                        }
                makeSquash _ = error "Invalid squash format in test vector"
             in Opened
                    (Amount amount)
                    (Snapshot (makeSquash squash1) (makeSquash squash2))
                    (Period $ DiffTimeMilliseconds period)
        _ -> error $ "Unsupported constructor: " ++ constructor

-- Property test helpers
prop_stageSerializationMatchesTestVector :: StageTestVector -> Property
prop_stageSerializationMatchesTestVector vector =
    let stage = testVectorToStage vector
        expected = cbor vector
     in -- actual = serializeStage stage -- You'll need to implement this
        counterexample
            ("Expected: " ++ expected ++ "\nActual: " {- ++ actual -})
            True -- (expected == actual)

-- Specs
spec :: Spec
spec = describe "Stage Serialization" $ do
    it "should match test vectors for Opened constructor" $ do
        pending "Implement test vector validation"
