{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main where

import           Databricks

import           Prelude               hiding (FilePath, lines)
import qualified Prelude               (FilePath)

import           Turtle                hiding (append)

import           Data.Monoid           (mempty, (<>))

import           Control.Monad         (forM_)
import           Control.Monad.Managed (MonadManaged)

import           Options.Applicative   as Opt

import           Data.Char             (isAscii)

import           Data.Maybe            (isNothing, catMaybes, fromJust)

import qualified Data.ByteString.Lazy  as B

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Control.Lens          hiding (view, (.=), (<.>))
import qualified Control.Lens          as Lens (view)

import qualified Data.HashMap.Lazy     as HM

import qualified Data.Vector           as V

import           Data.Aeson            ((.=))
import qualified Data.Aeson            as A
import qualified Data.Yaml             as Y
import           Data.Maybe (fromMaybe)

data Run = Run { runInfile :: FilePath, runOutpath :: FilePath }

infileP :: Parser FilePath
infileP = Opt.argument str (metavar "INPUT-FILE")

outpathP :: Parser FilePath
outpathP = Opt.argument str (metavar "OUTPUT-FILE")

run :: Parser Run
run = Run <$> infileP <*> outpathP

opts :: ParserInfo Run
opts = info (run <**> helper)
  ( fullDesc
  <> progDesc "Encloses htmlSandboxes in Databricks notebooks in <p> html tags with class htmlSandbox." )

isHtml :: Y.Value -> Bool
isHtml (Y.String value) = T.isInfixOf "htmlSandbox" value

setHtml :: Y.Value -> Y.Value
setHtml (Y.String value) = Y.String (T.concat ["<p class=\"htmlSandbox\">", value, "</p>"])
setHtml other = other

setHtmlMaybe :: Maybe Y.Value -> Maybe Y.Value
setHtmlMaybe value = setHtml <$> value

setLink :: DBResult -> DBResult
setLink result
  | fromMaybe False (isHtml <$> result^.dbrType) = over dbrData setHtmlMaybe result
  | otherwise                                    = result

setResult :: Maybe DBResult -> Maybe DBResult
setResult result = setLink <$> result

setCommand :: DBCommand -> DBCommand
setCommand = over dbcResults setResult

setNotebook :: DBNotebook -> DBNotebook
setNotebook = over dbnCommands (map setCommand)

isSourceFile :: Pattern Text
isSourceFile = choice [suffix s | s <- [".scala", ".python", ".R", ".sql"]]

gfp :: FilePath -> Prelude.FilePath
gfp = T.unpack . format fp

replacePrefix :: FilePath -> FilePath -> FilePath -> Maybe FilePath
replacePrefix old new xs = (new </>) <$> stripPrefix old xs

main :: IO ()
main = do
  (Run inFile outFile) <- execParser opts
  sh $ do
    srcFolder <- using (mktempdir "/tmp" "dbcflatten-source")
    _ <- procStrict "unzip" [format fp inFile, "-d", format fp srcFolder] mempty
    sh $ do
      resultFolder <- using (mktempdir "/tmp" "dbcflatten-result")
      cptree srcFolder resultFolder
      sh $ do
        srcFile <- find isSourceFile srcFolder
        liftIO $ print srcFile
        let Just newFile = replacePrefix (srcFolder </> "") (resultFolder </> "") srcFile
        liftIO $ print newFile
        srcContent <- liftIO (B.readFile (gfp srcFile))
        parsed <- either (die . T.pack) return (fromByteString srcContent)
        let transformed = setNotebook parsed
            final = toByteString transformed
            inFolder = directory inFile
        liftIO $ B.writeFile (gfp newFile) final
      _ <- procStrict "jar" [ "-Mcf", format fp outFile
                            , "-C", format fp resultFolder, "."] mempty
      return ()
