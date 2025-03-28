module Main where

import CardanoLightning.Commands.Build (
    buildCommandParser,
    runBuildCommand,
 )
import Control.Monad (join)
import Data.Foldable (Foldable (..))
import Data.Version (showVersion)
import Options.Applicative (
    InfoMod,
    Parser,
    ParserInfo,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    progDesc,
    short,
 )
import Paths_cardano_lightning (version)

commandParser :: Parser (IO ())
commandParser =
    hsubparser $
        fold
            [ command "build" $ runBuildCommand <$> buildCommandParser
            ]

appParser :: ParserInfo (IO ())
appParser = info (helper <*> versionOption <*> commandParser) description
  where
    versionOption :: Parser (a -> a)
    versionOption =
        infoOption
            ("cardano-cli" <> showVersion version)
            (long "version" <> short 'v' <> help "Show version.")

    description :: InfoMod (IO ())
    description =
        fold
            [ fullDesc
            , progDesc "CLI for Cardano Lightning Network"
            , header "cardano-lightning - CLI for Cardano Lightning Network"
            ]

main :: IO ()
main = join (execParser appParser)
