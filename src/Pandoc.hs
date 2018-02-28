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
import qualified Text.Pandoc.Class as P

import Data.Default (def)

import Control.Lens

import qualified Data.Sequence as S

import Notebook as N
import Utils

import Data.Text as T
import Data.Text.Encoding as E

import Data.List (sortOn)

getPosition :: N.Command -> Double
getPosition c = c^.cPosition

fromNotebook :: N.Notebook -> P.Pandoc
fromNotebook nb = P.setTitle title $ P.doc $ foldMap block (sortOn getPosition (nb^.nCommands))
  where title = P.text (T.unpack (nb^.nName))
        block c | c^.cLanguage == "md" =
                  let parsed = P.runPure $ P.readMarkdown ( def { P.readerExtensions = P.extensionsFromList [P.Ext_raw_html, P.Ext_raw_tex, P.Ext_hard_line_breaks] } ) (c^.cCommand)
                      P.Pandoc _ bs = either (error . show) id parsed
                  in blocks bs
                | otherwise =
                  let code = if c^.cCommandHidden || (T.null (c^.cCommand))
                             then mempty
                             else (P.codeBlockWith ("", [T.unpack $ c^.cLanguage], []) (T.unpack (c^.cCommand))) <> P.para (P.linebreak)
                      result = if c^.cResultHidden
                               then mempty
                               else maybe mempty (<> P.para (P.linebreak)) (N.success c)
                  in code <> result

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "This error is not handled"

toMarkdown :: P.Pandoc -> B.ByteString
-- toMarkdown = B.pack . P.writeMarkdown (def { P.writerExtensions = P.githubMarkdownExtensions })
toMarkdown = B.fromStrict . E.encodeUtf8 . fromRight . P.runPure . P.writeMarkdown (def { P.writerExtensions = P.enableExtension P.Ext_hard_line_breaks P.githubMarkdownExtensions, P.writerWrapText = P.WrapPreserve })

toHtml :: P.Pandoc -> B.ByteString
toHtml = B.fromStrict . E.encodeUtf8 . fromRight . P.runPure . P.writeHtml5String def

toNative :: P.Pandoc -> B.ByteString
toNative = B.fromStrict . E.encodeUtf8 . fromRight . P.runPure . P.writeNative def
