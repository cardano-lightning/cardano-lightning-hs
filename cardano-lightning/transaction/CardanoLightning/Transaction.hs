{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CardanoLightning.Transaction (
    CLError,
    CLTransactionArgs (..),
    LedgerInfo (..),
    Opening,
    SortedSteps,
    buildTxBody,
    mkOpening,
    mkSortedSteps,
)
where

import Cardano.Api (TxBodyContent (..))
import qualified Cardano.Api as C
import qualified Cardano.Api.Ledger as CL
import qualified Cardano.Api.Shelley as CS
import CardanoLightning.Contrib.Cardano.Api (AUTxO (..), foldToUTxO)
import qualified CardanoLightning.Contrib.Cardano.Api as A
import CardanoLightning.Transaction.Validator (VerificationKey)
import qualified CardanoLightning.Transaction.Validator as V
import Control.Error.Util (note)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import qualified PlutusTx as P

newtype SortedSteps = SortedSteps [(AUTxO C.ConwayEra, V.NStep)]

instance Semigroup SortedSteps where
    SortedSteps xs <> SortedSteps ys = SortedSteps $ sortBy (comparing (aTxIn . fst)) (xs <> ys)

instance Monoid SortedSteps where
    mempty = SortedSteps []

mkSortedSteps :: [(AUTxO C.ConwayEra, V.NStep)] -> SortedSteps
mkSortedSteps = SortedSteps . sortBy (comparing (aTxIn . fst))

data LedgerInfo = LedgerInfo
    { eraHistory :: C.EraHistory
    , networkId :: C.NetworkId
    , protocolParams :: CS.LedgerProtocolParameters C.ConwayEra
    , systemStart :: C.SystemStart
    }

data CLTransactionArgs = CLTransactionArgs
    { feeUTxO :: AUTxO C.ConwayEra
    -- ^ TODO: improve balancing/coin selection
    , clScriptUTxO :: AUTxO C.ConwayEra
    -- ^ Reference input which contains the CL script
    , ledgerInfo :: LedgerInfo
    , openings :: [Opening]
    -- ^ TODO: Simplified opening - no gifts support yet
    , steps :: SortedSteps
    }

data Opening
    = Opening
        VerificationKey
        VerificationKey
        C.Lovelace
        V.Period

mkOpening :: VerificationKey -> VerificationKey -> C.Lovelace -> V.Period -> Maybe Opening
mkOpening vk1 vk2 amount period
    | amount >= 0 = Just $ Opening vk1 vk2 amount period
    | otherwise = Nothing

-- TODO: For now let's keep the error type simple. We will introduce a more
-- precise error type later.
type CLError = Text

-- The result of a step execution:

-- * Can close the channel - so there is no new channel state.

-- * Can transition the channel - so there is a new channel state.

-- * In the first case we should also generate a payout as a side effect.

-- * In the second case depending on the step we can also generate a payout.
scriptHashFromReferenceInput :: C.TxOut C.CtxUTxO C.ConwayEra -> Maybe C.ScriptHash
scriptHashFromReferenceInput (C.TxOut _ _ _ possibleScriptReference) = case possibleScriptReference of
    CS.ReferenceScript
        _
        (C.ScriptInAnyLang _ (CS.PlutusScript CS.PlutusScriptV3 script)) -> Just $ hashPlutusV3Script script
    _ -> Nothing

mkStep :: V.Datum -> V.NStep -> (Maybe V.Datum, Maybe (C.Address C.ShelleyAddr, C.Lovelace))
mkStep = undefined

-- Our transaction is a bit unusual - the whole design is focused on batch execution:

-- * We treat the transaction as a list of channels which transition from one state to another or are openning or closed.

-- * In other words we iterate over the list of paired inputs and outputs and validate those pairs.

-- * This folding happens on the minting policy level because it is more efficient to do so.

-- * In order to safely delegate the validation to the minting policy we store the `ownHash` (the same script serves to purposes) in the datum.

--   (Minting policy on Cardano has access to the own hash but spending validators have to discover it by iterating over the inputs which is inefficient.)

-- * Now the spending scripts in their datum should have a triple of the form: `(ownHash, amount, stage)` (Validator.Datum type) - that what should be decoded from the datum of the AUTxO

-- * The miniting policy on the other hand accepts through the Validator.MintRedeemer a list of steps where each step corresponds to the inputs.

-- * On cardano ledger we can not control the order of the inputs - ledger sorts the inputs by the `TxIn` which is in essence "(TxId, TxId)".

-- * Given the above we have to rather adjust the order of outputs of the transaction based on the inputs which we want to spend.

-- * Additionally all the new channels should be outputted as the first outputs - we can start the list by creating those.

-- * Given all of the above:

--    * We should start by taking the list of openings and creating the first outputs.
--    * Then we should sort the list of steps based on the inputs.
--    * Then we should iterate over the list of steps and validate each step.
--    * Then we should decode datums from the sorted list of steps - `AUTxO` is in essence isomorphic to a pair of `C.TxIn` and `C.TxOut`. `C.TxOut` --      contains the datum.
--    * Then we should based on the datums and step create a possibly new step - we will elaborate on this later. For
buildTxBody :: CLTransactionArgs -> Either CLError (C.TxBody C.ConwayEra)
buildTxBody CLTransactionArgs{..} = do
    let LedgerInfo{..} = ledgerInfo
        AUTxO feeTxIn (C.TxOut changeAddress _ _ _) = feeUTxO
        SortedSteps rawSteps = steps
        AUTxO clScriptTxIn clScriptTxOut = clScriptUTxO

    -- \* Inputs
    let txIns = [(feeTxIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
    -- FIXME:
    -- [ (txIn, C.makeScriptWitness txIn scriptHash redeemer)
    -- \| (AUTxO txIn _, redeemer) <- sortedSteps
    -- ]
    (clScriptHash :: C.ScriptHash) <- note "Failed to extract script hash from reference input" (scriptHashFromReferenceInput clScriptTxOut)

    let
        -- Let's pick first steps txIn or if they are empty the feeTxIn
        mintingSeed = V.MintingSeed $ case steps of
            SortedSteps ((AUTxO txIn _, _) : _) -> txIn
            _ -> feeTxIn

        openings' :: [(Opening, V.ChannelId)]
        openings' = attachChannelIds mintingSeed openings

        -- FIXME: provide steps in here
        mintRedeemer = V.MintRedeemer{seed = Just mintingSeed, steps = []}
        scriptWitness =
            C.PlutusScriptWitness
                C.PlutusScriptV3InConway
                C.PlutusScriptV3
                (CS.PReferenceScript (aTxIn clScriptUTxO))
                C.NoScriptDatumForMint
                (makeHashableScriptData mintRedeemer)
                (C.ExecutionUnits 0 0)

        txMintValue = C.TxMintValue
            C.MaryEraOnwardsConway
            do
                M.singleton
                    (C.PolicyId clScriptHash)
                    [ (C.AssetName cid, 1, C.BuildTxWith scriptWitness)
                    | (_, V.ChannelId cid) <- openings'
                    ]

        openingsTxOuts = createOpeningOutputs networkId clScriptHash openings'
        -- FIXME: Add steps txOuts
        txOuts = openingsTxOuts

    C.BalancedTxBody _txBodyContent txBody _txBalanceOutput _fee <-
        first (mappend "TxBodyErrorAutoBalance" . T.pack . show) $
            makeTransactionBodyAutoBalance'
                ledgerInfo
                (feeUTxO : map fst rawSteps)
                ( (C.defaultTxBodyContent C.ShelleyBasedEraConway)
                    { txIns = txIns
                    , txInsCollateral = C.TxInsCollateral C.AlonzoEraOnwardsConway [feeTxIn]
                    , txOuts
                    , txMintValue
                    , txProtocolParams = C.BuildTxWith (Just protocolParams)
                    }
                )
                changeAddress
    pure txBody

deserializeDatumInline ::
    (P.FromData datum) => C.TxOutDatum ctx era -> Either String datum
deserializeDatumInline =
    \case
        C.TxOutDatumInline _ hashableScriptData ->
            maybe (Left "InvalidInputDatum") pure
                . P.fromData
                . CS.toPlutusData
                . CS.getScriptData
                $ hashableScriptData
        _ -> Left "MissingInputDatum"

getInlineDatum :: AUTxO era -> Either String V.Datum
getInlineDatum (AUTxO _ (C.TxOut _ _ datum _)) = deserializeDatumInline datum

createOpeningOutputs :: C.NetworkId -> C.ScriptHash -> [(Opening, V.ChannelId)] -> [C.TxOut C.CtxTx C.ConwayEra]
createOpeningOutputs networkId clScriptHash = do
    let
        clScriptAddress =
            C.AddressInEra
                (CS.ShelleyAddressInEra CS.ShelleyBasedEraConway)
                ( CS.makeShelleyAddress
                    networkId
                    (C.PaymentCredentialByScript clScriptHash)
                    C.NoStakeAddress
                )
        clPolicyId = C.PolicyId clScriptHash

    map $ uncurry \(Opening vk1 vk2 lovelace@(CL.Coin amount) period) (V.ChannelId channelId) -> do
        let
            channelIdAsset = C.AssetId clPolicyId (C.AssetName channelId)
            txOutValue =
                valueToTxOutValue $
                    C.valueFromList
                        [ (channelIdAsset, C.Quantity 1)
                        , (C.AdaAssetId, C.lovelaceToQuantity lovelace)
                        ]
            datum =
                C.TxOutDatumInline C.BabbageEraOnwardsConway . makeHashableScriptData $
                    V.Datum
                        { scriptHash = clScriptHash
                        , keys = V.Keys vk1 vk2
                        , stage = V.Opened (V.Amount amount) V.emptySnapshot period
                        }
        C.TxOut
            clScriptAddress
            txOutValue
            datum
            CS.ReferenceScriptNone

attachChannelIds :: V.MintingSeed -> [Opening] -> [(Opening, V.ChannelId)]
attachChannelIds mintingSeed openings = do
    (ix, opening) <- zip [0 ..] openings
    let channelId = V.mkChannelId mintingSeed (V.OpeningIx ix)
    pure (opening, channelId)

makeHashableScriptData :: (P.ToData a) => a -> C.HashableScriptData
makeHashableScriptData = C.unsafeHashableScriptData . CS.fromPlutusData . P.toData

hashPlutusV3Script :: C.PlutusScript C.PlutusScriptV3 -> C.ScriptHash
hashPlutusV3Script = C.hashScript . C.PlutusScript C.PlutusScriptV3

valueToTxOutValue :: C.Value -> C.TxOutValue C.ConwayEra
valueToTxOutValue = C.TxOutValueShelleyBased C.ShelleyBasedEraConway . CS.toMaryValue

makeTransactionBodyAutoBalance' ::
    LedgerInfo ->
    [AUTxO C.ConwayEra] ->
    C.TxBodyContent C.BuildTx C.ConwayEra ->
    C.AddressInEra C.ConwayEra ->
    Either (C.TxBodyErrorAutoBalance C.ConwayEra) (C.BalancedTxBody C.ConwayEra)
makeTransactionBodyAutoBalance' LedgerInfo{..} inputs txBodyContent changeAddress =
    C.makeTransactionBodyAutoBalance
        C.ShelleyBasedEraConway
        systemStart
        (C.toLedgerEpochInfo eraHistory)
        protocolParams
        mempty -- No stake pools being unregistered
        mempty -- No stake credential deposits being unregistered
        mempty -- No DRep credential deposits being unregistered
        (foldToUTxO inputs)
        txBodyContent
        changeAddress
        Nothing -- No override for key witnesses
