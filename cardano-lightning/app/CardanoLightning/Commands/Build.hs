module CardanoLightning.Commands.Build where

import qualified Cardano.Api as C
import qualified Cardano.Api.Ledger as CL
import qualified Cardano.Api.Network as CN
import qualified Cardano.Api.Shelley as CS
import qualified CardanoLightning.Contrib.Cardano.Api as CC
import qualified CardanoLightning.Transaction as T
import qualified CardanoLightning.Transaction.Validator as T
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Traversable (for)
import Options.Applicative (Parser, ReadM, asum, auto, eitherReader, flag', help, info, long, many, metavar, option, optional, progDesc, short, strOption)
import qualified Options.Applicative as O
import System.Exit (die)
import System.FilePath ((</>))
import Text.Read (readEither)

data BuildCommand = BuildCommand
    { clScriptTxIn :: C.TxIn
    -- ^ UTxO containing the CL reference script
    , feeTxIn :: C.TxIn
    -- ^ Input for fees/collateral
    , networkId :: CS.NetworkId
    -- ^ We need networkId upfront in order to query the node for the rest of the info
    , nodeSocketPath :: FilePath
    -- ^ Path to node socket
    , openings :: [(T.VerificationKey, T.VerificationKey, C.Lovelace, T.Period)]
    -- ^ Openings passed directly through the CLI
    , outDir :: FilePath
    -- ^ Output directory for results
    , stepsFile :: Maybe FilePath
    -- ^ JSON file containing the steps
    }

networkIdParser :: Parser CS.NetworkId
networkIdParser =
    asum
        [ flag' C.Mainnet $
            fold
                [ long "mainnet"
                , help "Connect to a mainnet node."
                ]
        , C.Testnet . C.NetworkMagic
            <$> option
                auto
                ( fold
                    [ long "testnet-magic"
                    , help "Connect to a testnet node with the given network magic."
                    ]
                )
        ]

nodeSocketPathParser :: Parser FilePath
nodeSocketPathParser =
    strOption $
        fold
            [ long "node-socket-file"
            , metavar "FILE_PATH"
            , help "The path to the node socket."
            ]

outDirParser :: Parser FilePath
outDirParser =
    strOption $
        fold
            [ long "out-dir"
            , short 'o'
            , metavar "DIR"
            , help "A directory to which all command outputs will be written."
            ]

clScriptTxInParser :: Parser C.TxIn
clScriptTxInParser =
    option readTxIn $
        fold
            [ long "cl-script-utxo"
            , metavar "TX_ID#TX_IX"
            , help "A tx in pointing to the UTxO containing the CL reference script."
            ]

-- | Parser for the build command
buildCommandParser :: O.ParserInfo BuildCommand
buildCommandParser =
    info
        ( BuildCommand
            <$> clScriptTxInParser
            <*> feeTxInParser
            <*> networkIdParser
            <*> nodeSocketPathParser
            <*> many openingParser
            <*> outDirParser
            <*> optional stepsFileParser
        )
        (progDesc "Build a transaction executing channel steps")

parseHex :: Text -> Either String ByteString
parseHex =
    first (const "Invalid hexadecimal text")
        . Base16.decode
        . Text.encodeUtf8

parseTxIn :: Text -> Either String C.TxIn
parseTxIn raw =
    do
        let (txIdStr, rest) = Text.splitAt 64 raw
        txIdBytes <- parseHex txIdStr
        txId <-
            first C.unSerialiseAsRawBytesError $ C.deserialiseFromRawBytes C.AsTxId txIdBytes
        case Text.uncons rest of
            Just ('#', txIxStr) -> do
                txIxWord <- readEither (Text.unpack txIxStr)
                pure $ C.TxIn txId $ C.TxIx txIxWord
            _ -> Left "Expected \"#<TX_IX>\""

readTxIn :: ReadM C.TxIn
readTxIn = eitherReader (parseTxIn . Text.pack)

feeTxInParser :: Parser C.TxIn
feeTxInParser =
    option readTxIn $
        fold
            [ long "fee-input"
            , help "A tx in to use for collateral and to cover fees."
            , metavar "TX_ID#TX_IX"
            ]

stepsFileParser :: O.Parser FilePath
stepsFileParser =
    strOption $
        fold
            [ long "steps-file"
            , metavar "FILE"
            , help "JSON file containing the steps to execute"
            ]

readVerificationKey :: ReadM T.VerificationKey
readVerificationKey = do
    let
        parser txt = do
            bytes <- parseHex txt
            vk <- first (const "Invalid verification key") $ C.deserialiseFromRawBytes (C.AsVerificationKey C.AsPaymentKey) bytes
            pure $ T.VerificationKey vk
    eitherReader (parser . Text.pack)

verificationKeyParser :: String -> O.Parser T.VerificationKey
verificationKeyParser optionName =
    O.option readVerificationKey $
        fold
            [ long optionName
            , metavar "HEX"
            , help "Hex-encoded Ed25519 verification key"
            ]

openingParser :: O.Parser (T.VerificationKey, T.VerificationKey, C.Lovelace, T.Period)
openingParser =
    (,,,)
        <$> verificationKeyParser "opener-key"
        <*> verificationKeyParser "non-opener-key"
        <*> do
            CL.Coin
                <$> option
                    auto
                    ( long "amount"
                        <> metavar "LOVELACE"
                        <> help "Amount to open channel with"
                    )
        <*> do
            let
                hoursToPeriod = T.Period . T.DiffTimeMilliseconds . (60 * 60 * 1000 *)
            hoursToPeriod
                <$> option
                    auto
                    ( long "contestation-period"
                        <> metavar "Hours"
                        <> help "Contestation period in hours"
                    )

mkOpening :: (T.VerificationKey, T.VerificationKey, C.Lovelace, T.Period) -> IO T.Opening
mkOpening (openerKey, nonOpenerKey, amount, contestationPeriod) = do
    case T.mkOpening openerKey nonOpenerKey amount contestationPeriod of
        Just opening -> pure opening
        Nothing -> die "Failed to create opening - is the amount negative?"

-- | Run the build command
runBuildCommand :: BuildCommand -> IO ()
runBuildCommand cmd = do
    let BuildCommand{clScriptTxIn, feeTxIn, openings, networkId, nodeSocketPath, outDir, stepsFile = possibleStepsFile} = cmd

    steps <- case possibleStepsFile of
        Nothing -> pure []
        Just stepsFile -> A.eitherDecodeFileStrict stepsFile >>= either die pure

    -- Connect to node
    let connectInfo =
            C.LocalNodeConnectInfo
                (C.CardanoModeParams (C.EpochSlots 21600))
                networkId
                (C.File nodeSocketPath)

    -- Query node for necessary info
    Right (Right systemStart, Right eraHistory, Right (Right protocolParams)) <-
        C.executeLocalStateQueryExpr connectInfo CN.VolatileTip $
            (,,)
                <$> C.querySystemStart
                <*> C.queryEraHistory
                <*> C.queryProtocolParameters CS.ShelleyBasedEraConway

    feeUTxO <- queryUTxO connectInfo feeTxIn
    clScriptUTxO <- queryUTxO connectInfo clScriptTxIn

    openings' <- for openings mkOpening

    -- Build transaction
    let args =
            T.CLTransactionArgs
                { feeUTxO = feeUTxO
                , clScriptUTxO = clScriptUTxO
                , ledgerInfo =
                    T.LedgerInfo
                        { eraHistory = eraHistory
                        , networkId = networkId
                        , protocolParams = CS.LedgerProtocolParameters protocolParams
                        , systemStart = systemStart
                        }
                , openings = openings'
                , steps = T.mkSortedSteps steps
                }

    case T.buildTxBody args of
        Left err -> die $ "Failed to build transaction: " <> show err
        Right txBody -> do
            -- Write transaction body
            let txFile = outDir </> "transaction.body"
            C.writeFileTextEnvelope (C.File txFile) Nothing txBody
                >>= either (die . show) pure

readVerificationKeyFile :: FilePath -> IO T.VerificationKey
readVerificationKeyFile file = do
    result <- C.readFileTextEnvelope (C.AsVerificationKey C.AsPaymentKey) (C.File file)
    case result of
        Left err -> die $ "Failed to read verification key: " <> show err
        Right key -> pure $ T.VerificationKey key

queryUTxO :: C.LocalNodeConnectInfo -> C.TxIn -> IO (CC.AUTxO C.ConwayEra)
queryUTxO connectInfo txIn = do
    result <-
        C.executeLocalStateQueryExpr connectInfo CN.VolatileTip $
            C.queryUtxo C.ShelleyBasedEraConway (C.QueryUTxOByTxIn (Set.singleton txIn))
    case result of
        Right (Right (Right (C.UTxO utxoMap))) ->
            case Map.lookup txIn utxoMap of
                Just txOut -> pure $ CC.AUTxO txIn txOut
                Nothing -> die $ "UTxO not found: " <> show txIn
        _ -> die "Failed to query UTxO"
