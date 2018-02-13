{-# LANGUAGE OverloadedStrings #-}
module Pandoc where

-- import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Lazy as B hiding (pack)
-- import qualified Data.ByteString.Lazy.Char8 as B

import qualified Text.Pandoc.Builder as P
import Text.Pandoc.Builder ((<>))
import qualified Text.Pandoc.Options as P
import qualified Text.Pandoc.Readers.Markdown as P
import qualified Text.Pandoc.Writers.Markdown as P
--import qualified Text.Pandoc.Readers.HTML as P
import qualified Text.Pandoc.Writers.HTML as P
import qualified Text.Pandoc.Writers.Native as P

import Data.Default (def)

import Control.Lens

import qualified Data.Sequence as S

import Notebook as N
import Utils

import Data.Text as T
import Data.Encoding.UTF8 as UTF8
import Data.Encoding as E

import qualified Data.Set as S

fromNotebook :: N.Notebook -> P.Pandoc
fromNotebook nb = P.setTitle title $ P.doc $ foldMap block (nb^.nCommands)
  where title = P.text (T.unpack (nb^.nName))
        block c | c^.cLanguage == "md" =
                  let parsed = P.readMarkdown def (T.unpack (c^.cCommand))
                      P.Pandoc _ bs = either (error . show) id parsed
                  in blocks bs
                | otherwise =
                  let code = if c^.cCommandHidden
                             then mempty
                             else (P.codeBlockWith ("", [T.unpack $ c^.cLanguage], []) (T.unpack (c^.cCommand))) <> P.para (P.linebreak)
                      result = if c^.cResultHidden
                               then mempty
                               else maybe mempty (<> P.para (P.linebreak)) (N.success c)
                  in code <> result

toMarkdown :: P.Pandoc -> B.ByteString
-- toMarkdown = B.pack . P.writeMarkdown (def { P.writerExtensions = P.githubMarkdownExtensions })
toMarkdown =  (E.encodeLazyByteString UTF8.UTF8). P.writeMarkdown (def { P.writerExtensions = S.insert P.Ext_hard_line_breaks P.githubMarkdownExtensions, P.writerWrapText = P.WrapPreserve })

toHtml :: P.Pandoc -> B.ByteString
toHtml = (E.encodeLazyByteString UTF8.UTF8) . P.writeHtmlString (def { P.writerHtml5 = True })

toNative :: P.Pandoc -> B.ByteString
toNative = (E.encodeLazyByteString UTF8.UTF8) . P.writeNative def
