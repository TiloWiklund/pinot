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

import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as B

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Control.Lens          hiding (view, (.=), (<.>))
import qualified Control.Lens          as Lens (view)

import qualified Data.HashMap.Lazy     as HM

import qualified Data.Vector           as V

import           Data.Aeson            ((.=))
import qualified Data.Aeson            as A
import qualified Data.Yaml             as Y

-- import           Filesystem.Path      (directory)

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
  <> progDesc "Flattens iframes in Databricks notebooks by removing the iframe and inserting a link to the URL of the iframe." )

isIframe :: Text -> Bool
isIframe = T.isInfixOf "<iframe"

getLink :: Text -> Text
getLink input = T.concat ["<a href=\"", url, "\">", url, "</a>"]
  where url = fst $ T.breakOn "\"" (T.drop 5 (snd $ T.breakOn "src" input))

setLink :: Y.Value -> Y.Value
setLink (Y.String input)
  | isIframe input = Y.String (getLink input)
  | otherwise      = Y.String input
setLink other = other

setLinkMaybe :: Maybe Y.Value -> Maybe Y.Value
setLinkMaybe x = setLink <$> x

setResult :: Maybe DBResult -> Maybe DBResult
setResult result = (over dbrData setLinkMaybe) <$> result

setCommand :: DBCommand -> DBCommand
setCommand = over dbcResults setResult

setNotebook :: DBNotebook -> DBNotebook
setNotebook = over dbnCommands (map setCommand)

isSourceFile :: Pattern Text
isSourceFile = choice [suffix s | s <- [".scala", ".py", ".R", ".sql"]]

gfp :: FilePath -> Prelude.FilePath
gfp = T.unpack . format fp

replacePrefix :: FilePath -> FilePath -> FilePath -> Maybe FilePath
replacePrefix old new xs = (new </>) <$> stripPrefix old xs

main :: IO ()
main = do
  (Run infile outfile) <- execParser opts
  sh $ do
    srcfolder <- using (mktempdir "/tmp" "dbcflatten-source")
    _ <- procStrict "unzip" [format fp infile, "-d", format fp srcfolder] mempty
    sh $ do
      resultFolder <- using (mktempdir "/tmp" "dbcflatten-result")
      sh $ do
        srcfile <- find isSourceFile srcfolder
        liftIO $ print srcfile
        let Just newfile = replacePrefix (srcfolder </> "") (resultFolder </> "") srcfile
        liftIO $ print newfile
        srcontent <- liftIO (B.readFile (gfp srcfile))
        parsed <- either (die . T.pack) return (fromByteString srcontent)
        let transformed = setNotebook parsed
            final = toByteString transformed
            inFolder = directory infile
        liftIO $ B.writeFile (gfp newfile) final
      _ <- procStrict "jar" [ "-Mcf", format fp outfile
                            , "-C", format fp resultFolder, "."] mempty
      return ()
