{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Default (def)
-- import qualified Text.Pandoc.Writers.Native as P
-- import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import Control.Monad (when, forM_)
import System.FilePath ((</>))

import Control.Lens

import Zeppelin as Z
import Databricks as D
import Notebook as N
import Pandoc as P
import Utils
import Formats

import Options.Applicative as Opt

sourceFormat :: Parser SourceFormat
sourceFormat = parseFormat <$> sourceFormat'
  where parseFormat "databricks"      = databricksDBCSource
        parseFormat "databricks-json" = databricksJSONSource
        parseFormat "zeppelin"        = zeppelinSource
        parseFormat _ = error "Unknown source format"
        sourceFormat' = strOption ( long "from"
                                  <> short 'f'
                                  <> metavar "FROM"
                                  <> help "Format to convert from" )

targetFormat :: Parser TargetFormat
targetFormat = parseFormat <$> targetFormat'
  where parseFormat "databricks-json" = databricksJSONTarget
        parseFormat "zeppelin"        = zeppelinTarget
        parseFormat "markdown"        = markdownTarget
        parseFormat "markdown-katex"  = markdownTargetKaTeX
        parseFormat "html"            = htmlTarget
        parseFormat "pandoc"          = pandocTarget
        parseFormat _ = error "Unknown target format"
        targetFormat' = strOption ( long "to"
                                    <> short 't'
                                    <> metavar "TO"
                                    <> help "Format to convert to" )

inputPaths :: Parser [FilePath]
inputPaths = some (Opt.argument str (metavar "INPUTS..."))

outputPath :: Parser FilePath
outputPath = strOption (long "out" <> short 'o' <> metavar "OUTPUT")

data Run = Run { from :: SourceFormat, to :: TargetFormat, inputs :: [FilePath], output :: Maybe FilePath }

run :: Parser Run
run = Run <$> sourceFormat <*> targetFormat <*> inputPaths <*> optional outputPath

opts :: ParserInfo Run
opts = info (run <**> helper)
  ( fullDesc
  <> progDesc "Convert between different notebook formats" )

main  :: IO ()
main = do
  (Run from to inputs output) <- execParser opts
  inputStreams <- if null inputs
                  then do stream <- B.getContents
                          return [("stdin", stream)]
                  else do streams <- mapM B.readFile inputs
                          return (zip inputs streams)
  let attempt = to <$> concatMapM (uncurry from) inputStreams
      results  = either (error . show) id attempt

  when (null results) (error "Nothing to output.")

  case output of
    Nothing -> case results of
      [(_, x)] -> B.putStrLn x
      _ -> error "Cannot output multiple files to stdout"
    Just o -> forM_ results $ \(f, x) -> do
      ensureCanBeCreated (o </> f)
      B.writeFile (o </> f) x
