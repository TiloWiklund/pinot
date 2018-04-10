{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter where

import Data.Default (def)
import qualified Data.Default as D
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Traversable (mapAccumL)
import Data.Aeson
import Data.Aeson.Types (Pair, Series)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
-- import qualified Text.Pandoc.Builder as P
import Control.Lens hiding ((.=))
import Data.Default
import Utils
import qualified Notebook as N

data JupyterFormat = JF { _majorVersion :: Int, _minorVersion :: Int }
  deriving Show

instance D.Default JupyterFormat where
  def = JF 4 0

data JupyterNotebook = JN { _jnCells :: [JupyterCell]
                          , _jnFormat :: JupyterFormat
                          , _jnKernel :: Maybe JupyterKernel
                          , _jnLanguage :: Maybe JupyterLanguage
                          , _jnAuthors :: [Text] }
  deriving Show

instance D.Default JupyterNotebook where
  def = JN def def def def def

data JupyterKernel = JK { _jkName :: Text }
  deriving Show

-- instance D.Default JupyterKernel where
--   def = JK ""

data JupyterLanguage = JL { _jlName :: Text
                          , _jlVersion :: Maybe Text
                          , _jlCodeMirrorMode :: Maybe Text }
  deriving Show

-- instance D.Default JupyterLanguage where
--   def = JL ""

type MIME = Text
data Autoscroll = AutoscrollSet Bool
                | AutoscrollAuto
  deriving Show

-- TODO: Add an other/unknown cell type
data JupyterCell = CMarkdown { _jcSource :: Text
                             , _jcCollapsed :: Bool
                             , _jcAutoscroll :: Autoscroll
                             , _jcDeletable :: Bool
                             , _jcName :: Text
                             , _jcSourceHidden :: Bool
                             , _jcOutputHidden :: Bool
                             , _jcTags :: [Text] }
                 | CCode { _jcSource :: Text
                         , _jcExecutionCount :: Maybe Int
                         , _jcCollapsed :: Bool
                         , _jcAutoscroll :: Autoscroll
                         , _jcDeletable :: Bool
                         , _jcName :: Text
                         , _jcTags :: [Text]
                         , _jcSourceHidden :: Bool
                         , _jcOutputHidden :: Bool
                         , _jcOutputs :: [JupyterOutput] }
                 | CRaw { _jcSource :: Text
                        , _jcCollapsed :: Bool
                        , _jcAutoscroll :: Autoscroll
                        , _jcDeletable :: Bool
                        , _jcName :: Text
                        , _jcTags :: [Text]
                        , _jcSourceHidden :: Bool
                        , _jcOutputHidden :: Bool
                        -- TODO: Is there a MIME-library?
                        , _jcFormat :: MIME }
  deriving Show

-- instance D.Default JupyterCell where
--   def = CMarkdown ""

type MIMEBundle = H.HashMap MIME (Value, Value)

data JupyterOutput = OStream { _joName :: Text
                             , _joTest :: Text
                             , _joIsolated :: Bool }
                   | ODisplay { _joDisplay :: MIMEBundle
                              , _joIsolated :: Bool }
                   | OResult { _joDisplay :: MIMEBundle
                             , _joExecCount :: Int
                             , _joIsolated :: Bool }
                   | OError { _joExceptionName :: Text
                            , _joExceptionValue :: Text
                            , _joTraceback :: [Text]
                            , _joIsolated :: Bool }
  deriving Show

makeLenses ''JupyterOutput
makeLenses ''JupyterCell
makeLenses ''JupyterLanguage
makeLenses ''JupyterKernel
makeLenses ''JupyterNotebook

-- instance ToJSON ZeppelinParagraph where
--   toEncoding zp = pairs ( "focus" .=? (zp^.zpFocus)
--                           <> "status" .=? (zp^.zpStatus)
--                           <> "apps" .=? (zp^.zpApps)
--                           <> "config" .=? (zp^.zpConfig)
--                           <> "progressUpdateIntervalMs"
--                               .=? (zp^.zpProgressUpdateIntervalMs)
--                           <> "settings" .=? (zp^.zpSettings)
--                           <> "text" .= (zp^.zpText)
--                           <> "jobName" .=? (zp^.zpJobName)
--                           <> "result" .=? (zp^.zpResult)
--                           <> "dateUpdated" .=? (zp^.zpDateUpdated)
--                           <> "dateCreated" .=? (zp^.zpDateCreated)
--                           <> "dateStarted" .=? (zp^.zpDateStarted)
--                           <> "dateFinished" .=? (zp^.zpDateFinished)
--                           <> "$$hashKey" .=? (zp^.zpHashKey)
--                           <> "id" .=? (zp^.zpId)
--                           <> "errorMessage" .=? (zp^.zpErrorMessage) )

--   toJSON zp = objectMaybe [ "focus" .=? (zp^.zpFocus)
--                           , "status" .=? (zp^.zpStatus)
--                           , "apps" .=? (zp^.zpApps)
--                           , "config" .=? (zp^.zpConfig)
--                           , "progressUpdateIntervalMs"
--                             .=? (zp^.zpProgressUpdateIntervalMs)
--                           , "settings" .=? (zp^.zpSettings)
--                           , "text" .= (zp^.zpText)
--                           , "jobName" .=? (zp^.zpJobName)
--                           , "result" .=? (zp^.zpResult)
--                           , "dateUpdated" .=? (zp^.zpDateUpdated)
--                           , "dateCreated" .=? (zp^.zpDateCreated)
--                           , "dateStarted" .=? (zp^.zpDateStarted)
--                           , "dateFinished" .=? (zp^.zpDateFinished)
--                           , "$$hashKey" .=? (zp^.zpHashKey)
--                           , "id" .=? (zp^.zpId)
--                           , "errorMessage" .=? (zp^.zpErrorMessage) ]

-- instance FromJSON ZeppelinParagraph where
--   parseJSON = withObject "Paragraph" $ \v -> ZP
--     <$> v .:? "focus"
--     <*> v .:? "status"
--     <*> v .:? "apps"
--     <*> v .:? "config"
--     <*> v .:? "progressUpdateIntervalMs"
--     <*> v .:? "settings"
--     <*> v .:  "text"
--     <*> v .:? "jobName"
--     <*> v .:? "result"
--     <*> v .:? "dateUpdated"
--     <*> v .:? "dateCreated"
--     <*> v .:? "dateStarted"
--     <*> v .:? "dateFinished"
--     <*> v .:? "$$hashKey"
--     <*> v .:? "id"
--     <*> v .:? "errorMessage"

-- instance ToJSON ZeppelinNotebook where
--   toEncoding zn = pairs ( "angularObjects" .=? (zn^.znAngularObjects)
--                           <> "config"      .=? (zn^.znConfig)
--                           <> "paragraphs"  .=  (zn^.znParagraphs)
--                           <> "name"        .=  (zn^.znName)
--                           <> "id"          .=? (zn^.znId)
--                           <> "info"        .=? (zn^.znInfo))

--   toJSON zn = objectMaybe [ "angularObjects" .=? (zn^.znAngularObjects)
--                           , "config"         .=? (zn^.znConfig)
--                           , "paragraphs"     .=  (zn^.znParagraphs)
--                           , "name"           .=  (zn^.znName)
--                           , "id"             .=? (zn^.znId)
--                           , "info"           .=? (zn^.znInfo) ]

-- instance FromJSON ZeppelinNotebook where
--   parseJSON = withObject "ZeppelinNotebook" $ \v -> ZN
--     <$> v .: "angularObjects"
--     <*> v .: "config"
--     <*> v .: "paragraphs"
--     <*> v .: "name"
--     <*> v .: "id"
--     <*> v .: "info"

-- instance ToJSON ZeppelinConfig where
--   toEncoding (ZC looknfeel) = pairs ("looknfeel" .= looknfeel)
--   toJSON (ZC looknfeel) = object [ "looknfeel" .= looknfeel ]

-- instance FromJSON ZeppelinConfig where
--   parseJSON = withObject "ZeppelinConfig" $ \v -> ZC <$> (v .: "looknfeel")

-- instance ToJSON ZeppelinInfo where
--   toEncoding zi = pairs mempty
--   toJSON zi = object []

-- instance FromJSON ZeppelinInfo where
--   parseJSON = withObject "ZeppelinInfo" (const (return (ZI ())))

-- fromByteString :: B.ByteString -> Either String ZeppelinNotebook
-- fromByteString = eitherDecode

-- toByteString :: ZeppelinNotebook -> B.ByteString
-- toByteString = encode

-- fromMDLanguage :: T.Text -> T.Text
-- fromMDLanguage "scala" = "spark"
-- fromMDLanguage x = x

-- fromNotebook :: N.Notebook -> ZeppelinNotebook
-- fromNotebook nb = defWith [ znName .~ (nb^.N.nName)
--                           , znParagraphs .~ map toZParagraph (nb^.N.nCommands) ]
--   where toZParagraph nc = defWith [zpText .~ addLang (nc^.N.cLanguage) (nc^.N.cCommand)]
--         addLang l c = T.unlines [ T.cons '%' (fromMDLanguage l), c ]

-- toNotebook :: ZeppelinNotebook -> N.Notebook
-- toNotebook zn = N.N (zn^.znName) (toCommands (zn^.znParagraphs))
--   where toCommands = snd . mapAccumL toCommand "md"
--         toCommand :: Text -> ZeppelinParagraph -> (Text, N.Command)
--         toCommand prev zp =
--           let (langTag, rawCommand) = splitLangTag (zp^.zpText) in
--           case langTag of
--             Nothing   -> (prev, N.C prev rawCommand Nothing False False)
--             Just lang -> (lang, N.C lang rawCommand Nothing False False)
--         splitLangTag unparsedCommand =
--           if maybe False (== '%') (unparsedCommand `safeIndex` 0)
--           then let (x:xs) = T.lines unparsedCommand
--                in (Just (T.stripEnd . T.tail $ x), T.unlines xs)
--           else (Nothing, unparsedCommand)

-- -- toPandoc :: ZeppelinNotebook -> P.Pandoc
-- -- toPandoc z = P.doc $ foldMap (P.codeBlock . unpack . pText) (znParagraphs z)

-- -- fromPandoc :: P.Pandoc -> ZeppelinNotebook
-- -- fromPandoc = undefined
