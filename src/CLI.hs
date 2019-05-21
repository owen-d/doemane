module CLI where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Config = Config
  { dictionary :: FilePath
  , quiet      :: Bool
  }
  deriving (Show)

config :: Parser Config
config = Config
      <$> strOption
      ( long "dict"
        <> short 'd'
        <> metavar "DICT"
        <> help "dictionary file to ingest" )
      <*> switch
      ( long "quiet"
        <> short 'q'
        <> help "Whether to be quiet" )

runConfig :: (Config -> IO ())-> IO ()
runConfig f = f =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "TODO: desc"
     <> header "hello - a test for optparse-applicative" )
