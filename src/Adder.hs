{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
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

format :: Parser NotebookFormat
format = parseFormat <$> format'
  where parseFormat "zeppelin" = zeppelinFormat
        parseFormat _ = error "Unknown target format"
        format' = strOption ( long "format"
                              <> short 'f'
                              <> metavar "FORMAT"
                              <> help "Format of the notebook" )

langTag :: T.Text -> T.Text
langTag command = if maybe False (== '%') ((T.toStrict command) `safeIndex` 0)
                  then T.drop 1 (head (T.lines command))
                  else "scala"

prependCode :: B.ByteString -> N.Notebook -> N.Notebook
prependCode c n = set nCommands (codeBlock : n^.nCommands) n
  where codeBlock = C (T.toStrict $ langTag (E.decodeUtf8 c)) (T.toStrict $ T.unlines $ tail $T.lines $ E.decodeUtf8 c) Nothing False False

prependCodes' :: B.ByteString -> [(String, N.Notebook)] -> [(String, N.Notebook)]
prependCodes' c nList = over (each . _2) (prependCode c) nList

prependCodes :: B.ByteString -> Either String [(String, N.Notebook)] -> Either String [(String, N.Notebook)]
prependCodes c nList = (prependCodes' c) <$> nList

targetFormat :: Parser TargetFormat
targetFormat = parseFormat <$> targetFormat'
  where parseFormat "zeppelin"        = zeppelinTarget
        parseFormat _ = error "Unknown target format"
        targetFormat' = strOption ( long "to"
                                    <> short 't'
                                    <> metavar "TO"
                                    <> help "Format to convert to" )

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

inputPaths :: Parser [FilePath]
inputPaths = some (Opt.argument str (metavar "INPUT NOTEBOOKS"))

outputPath :: Parser FilePath
outputPath = strOption (long "out" <> short 'o' <> metavar "OUTPUT")

inputCode :: Parser FilePath
inputCode = strOption (long "code" <> short 'c' <> metavar "CODE FILE" <> help "File containing code to prepend")

data Run = Run { inputFormat :: NotebookFormat, codeFile :: FilePath, inputs :: [FilePath], output :: Maybe FilePath }

run :: Parser Run
run = Run <$> format <*> inputCode <*> inputPaths <*> optional outputPath

opts :: ParserInfo Run
opts = info (run <**> helper)
  ( fullDesc
  <> progDesc "Adds the contents of the provided textfile as the first cell of the provided notebook." )

main  :: IO ()
main = do
  (Run (from, to) codefile inputs output) <- execParser opts
  inputStreams <- if null inputs
                  then do stream <- B.getContents
                          return [("stdin", stream)]
                  else do streams <- mapM B.readFile inputs
                          return (zip inputs streams)
  code <- B.readFile codefile
  let attempt = to <$> concatMapM ((prependCodes code) . (uncurry from)) inputStreams
      results  = either (error . show) id attempt

  when (null results) (error "Nothing to output.")

  case output of
    Nothing -> case results of
      [(_, x)] -> B.putStrLn x
      _ -> error "Cannot output multiple files to stdout"
    Just o -> forM_ results $ \(f, x) -> do
      ensureCanBeCreated (o </> f)
      B.writeFile (o </> f) x