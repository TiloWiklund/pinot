{-# LANGUAGE OverloadedStrings #-}
module Formats where

import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import Data.Monoid ((<>))
import Control.Monad (when, forM_)

import Control.Lens

import Zeppelin as Z
import Databricks as D
import Notebook as N
import Pandoc as P
import Utils

type SourceFormat = String -> B.ByteString -> Either String [(String, N.Notebook)]
type TargetFormat = [(String, N.Notebook)] -> [(String, B.ByteString)]
type NotebookFormat = (SourceFormat, TargetFormat)

databricksDBCSource :: SourceFormat
databricksDBCSource f x = over (each . _2) D.toNotebook <$> fromByteStringArchive x

databricksJSONSource :: SourceFormat
databricksJSONSource f x = (singleton . D.toNotebook) <$> D.fromByteString x
  where singleton y = [(f, y)]

zeppelinSource :: SourceFormat
zeppelinSource f x = (singleton . Z.toNotebook) <$> Z.fromByteString x
  where singleton y = [(f, y)]

databricksJSONTarget :: TargetFormat
databricksJSONTarget = over (each . _2) compile
  where compile = D.toByteString . D.fromNotebook

zeppelinTarget :: TargetFormat
zeppelinTarget = over (each . _1) (swapExtension ".json") . over (each . _2) compile
  where compile = Z.toByteString . Z.fromNotebook

markdownTarget :: TargetFormat
markdownTarget = over (each . _1) (swapExtension ".md") . over (each . _2) compile
  where compile = P.toMarkdown . P.fromNotebook

markdownTargetKaTeX :: TargetFormat
markdownTargetKaTeX = over (each . _1) (swapExtension ".md") . over (each . _2) compile
  where compile = P.toMarkdownKaTeX . P.fromNotebook

htmlTarget :: TargetFormat
htmlTarget = over (each . _1) (swapExtension ".html") . over (each . _2) compile
  where compile = P.toHtml . P.fromNotebook

pandocTarget :: TargetFormat
pandocTarget = over (each . _1) (swapExtension ".pandoc") . over (each . _2) compile
  where compile = P.toNative . P.fromNotebook

zeppelinFormat = (zeppelinSource, zeppelinTarget)
