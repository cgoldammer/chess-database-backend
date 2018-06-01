import Options.Applicative

import AppTypes
import qualified Test.Fixtures as F

dbTypeParser :: Parser String
dbTypeParser = fmap dbNameReader $ switch (long "dev" <> short 't' <> help "Run test analysis")

dbNameReader :: Bool -> String
dbNameReader True = "dev"
dbNameReader False = "prod"

main :: IO ()
main = do
  settings <- execParser opts
  F.runJob settings
  return ()

parseSettings :: Parser F.FixtureSettings
parseSettings = F.FixtureSettings
  <$> dbTypeParser
  <*> switch (long "runEval" <> short 'e' <> help "Run the evaluation")
  <*> switch (long "onlyContinueEval" <> short 'c' <> help "Only continue the evaluation")

opts = info (parseSettings <**> helper)
  ( fullDesc
  <> progDesc "Haskell-chess"
  <> header "" )

