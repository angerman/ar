module Main where

import Data.Ar
import System.Environment (getArgs)
import qualified Data.ByteString as B
import Options.Applicative
import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Data.List (isSuffixOf, partition)

data OutputType = BSD | GNU deriving (Show, Eq)
data Options = Options
  { outputMode :: OutputType
  , outputFile :: FilePath
  , inputFiles :: [FilePath]
  } deriving (Show, Eq)

mode :: Parser OutputType
mode =      (flag' BSD (long "bsd" <> short 'b' <> help "Produce a BSD archive"))
        <|> (flag' GNU (long "gnu" <> short 'g' <> help "Produce a GNU archive"))

out :: Parser FilePath
out = strOption
      ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Write output to FILE" )

args :: Parser Options
args = Options
  <$> mode <*> out <*> some (argument str (metavar "FILES..."))
      
main :: IO ()
main = driver =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

driver :: Options -> IO ()
driver opts = do
  let (archives, objects) = partition (isSuffixOf ".a") (inputFiles opts)
  ars <- mapM loadAr archives
  a   <- Archive <$> mapM loadObj objects
  case (outputMode opts) of
    BSD -> writeBSDAr (outputFile opts) (filtera (not . isBSDSymdef) (foldl (<>) a ars))
    GNU -> writeGNUAr (outputFile opts) (filtera (not . isGNUSymdef) (foldl (<>) a ars))
