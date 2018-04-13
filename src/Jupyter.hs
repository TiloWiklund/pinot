{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter where

import Data.Default (def)
import qualified Data.Default as D
import Data.Text (Text)
import qualified Data.Text as T (lines)
import Data.Aeson
-- import Data.Aeson.Types (Pair, Series)
import Data.Monoid ((<>))
-- import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as H
import Control.Lens hiding ((.=))
import Utils ((.=?), objectMaybe)
-- import qualified Notebook as N

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

-- instance D.Default JupyterNotebook where
--   def = JN def def def def def

newtype JupyterKernel = JK { _jkName :: Text }
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

newtype MIMEBundle = MIMEBundle (H.HashMap MIME Value)
  deriving Show
newtype MIMEMetadata = MIMEMetadata (H.HashMap MIME Value)
  deriving Show

-- TODO: Add an other/unknown cell type
data JupyterCell = CMarkdown { _jcSource :: Text
                             , _jcCollapsed :: Maybe Bool
                             , _jcAutoscroll :: Maybe Autoscroll
                             , _jcDeletable :: Maybe Bool
                             , _jcName :: Maybe Text
                             , _jcSourceHidden :: Maybe Bool
                             , _jcOutputHidden :: Maybe Bool
                             , _jcTags :: [Text]
                             , _jcAttachments :: Maybe MIMEBundle }
                 | CCode { _jcSource :: Text
                         , _jcExecutionCount :: Maybe Int
                         , _jcCollapsed :: Maybe Bool
                         , _jcAutoscroll :: Maybe Autoscroll
                         , _jcDeletable :: Maybe Bool
                         , _jcName :: Maybe Text
                         , _jcTags :: [Text]
                         , _jcSourceHidden :: Maybe Bool
                         , _jcOutputHidden :: Maybe Bool
                         , _jcOutputs :: [JupyterOutput] }
                 | CRaw { _jcSource :: Text
                        , _jcCollapsed :: Maybe Bool
                        , _jcAutoscroll :: Maybe Autoscroll
                        , _jcDeletable :: Maybe Bool
                        , _jcName :: Maybe Text
                        , _jcTags :: [Text]
                        , _jcSourceHidden :: Maybe Bool
                        , _jcOutputHidden :: Maybe Bool
                        -- TODO: Is there a MIME-library?
                        , _jcFormat :: Maybe MIME }
  deriving Show

-- instance D.Default JupyterCell where
--   def = CMarkdown ""

data JupyterOutput = OStream { _joName :: Text
                             , _joText :: Text
                             , _joIsolated :: Maybe Bool }
                   | ODisplay { _joDisplay :: MIMEBundle
                              , _joMetadata :: MIMEMetadata
                              , _joIsolated :: Maybe Bool }
                   | OResult { _joDisplay :: MIMEBundle
                             , _joMetadata :: MIMEMetadata
                             , _joExecCount :: Maybe Int
                             , _joIsolated :: Maybe Bool }
                   | OError { _joExceptionName :: Text
                            , _joExceptionValue :: Text
                            , _joTraceback :: [Text]
                            , _joIsolated :: Maybe Bool }
  deriving Show

-- jupyterOutputType :: JupyterOutput -> String
-- jupyterOutputType OStream {}  = "stream"
-- jupyterOutputType ODisplay {} = "display_data"
-- jupyterOutputType OResult {}  = "execute_result"
-- jupyterOutputType OError {}   = "error"

makeLenses ''JupyterOutput
makeLenses ''JupyterCell
makeLenses ''JupyterLanguage
makeLenses ''JupyterKernel
makeLenses ''JupyterNotebook

instance ToJSON MIMEBundle where
  toEncoding (MIMEBundle mb) = toEncoding mb
  toJSON (MIMEBundle mb) = toJSON mb

instance ToJSON MIMEMetadata where
  toEncoding (MIMEMetadata mb) = toEncoding mb
  toJSON (MIMEMetadata mb) = toJSON mb

instance FromJSON MIMEBundle where
  parseJSON j = MIMEBundle <$> parseJSON j

instance FromJSON MIMEMetadata where
  parseJSON j = MIMEMetadata <$> parseJSON j

asText :: Text -> Text
asText = id

instance ToJSON JupyterOutput where
  toEncoding (OStream name text isolated) =
    pairs ( ("output_type" .= asText "stream")
            <> ("name" .= name)
            <> ("text" .= text)
            <> ("isolated" .=? isolated) )
  toEncoding (ODisplay display metadata isolated) =
    pairs ( ("output_type" .= asText "display_data")
            <> ("data" .= display)
            <> ("metadata" .= metadata)
            <> ("isolated" .=? isolated) )
  toEncoding (OResult display metadata execCount isolated) =
    pairs ( ("output_type" .= asText "execute_result")
            <> ("data" .= display)
            <> ("metadata" .= metadata)
            <> ("execution_count" .= execCount)
            <> ("isolated" .=? isolated) )
  toEncoding (OError exceptionName exceptionValue traceback isolated) =
    pairs ( ("output_type" .= asText "error")
            <> ("ename" .= exceptionName)
            <> ("evalue" .= exceptionValue)
            <> ("traceback" .= traceback)
            <> ("isolated" .=? isolated) )

  toJSON (OStream name text isolated) =
    objectMaybe [ "output_type" .= asText "stream"
                , "name" .= name
                , "text" .= text
                , "isolated" .=? isolated ]

  toJSON (ODisplay display metadata isolated) =
    objectMaybe [ "output_type" .= asText "display_data"
                , "data" .= display
                , "metadata" .= metadata
                , "isolated" .=? isolated ]
  toJSON (OResult display metadata execCount isolated) =
    objectMaybe [ "output_type" .= asText "execute_result"
                , "data" .= display
                , "metadata" .= metadata
                , "execution_count" .= execCount
                , "isolated" .=? isolated ]
  toJSON (OError exceptionName exceptionValue traceback isolated) =
    objectMaybe [ "output_type" .= asText "error"
                , "ename" .= exceptionName
                , "evalue" .= exceptionValue
                , "traceback" .= traceback
                , "isolated" .=? isolated ]

instance FromJSON JupyterOutput where
  parseJSON = withObject "Output" $ \v -> do
    ot <- v .: "output_type"
    case ot of
      (String "stream") -> OStream
        <$> v .: "name"
        <*> v .: "text"
        <*> v .:? "isolated"
      (String "display_data") -> ODisplay
        <$> v .: "data"
        <*> v .: "metadata"
        <*> v .:? "isolated"
      (String "execute_result") -> OResult
        <$> v .: "data"
        <*> v .: "metadata"
        <*> v .:? "execution_count"
        <*> v .:? "isolated"
      (String "error") -> OError
        <$> v .: "ename"
        <*> v .: "evalue"
        <*> v .: "traceback"
        <*> v .:? "isolated"
      _ -> error "Unknown Jupyter output type"

instance ToJSON Autoscroll where
  toEncoding AutoscrollAuto = toEncoding (asText "auto")
  toEncoding (AutoscrollSet x) = toEncoding x
  toJSON AutoscrollAuto = toJSON (asText "auto")
  toJSON (AutoscrollSet x) = toJSON x

instance FromJSON Autoscroll where
  parseJSON (Bool x) = pure (AutoscrollSet x)
  parseJSON (String "auto") = pure AutoscrollAuto
  parseJSON _ = error "Unknown value for autoscroll"

cellMetadata :: JupyterCell -> Value
cellMetadata jc = Object $
  mconcat [ singleMaybe "collapsed" (jc^.jcCollapsed)
          , singleMaybe "autoscroll" (jc^.jcAutoscroll)
          , singleMaybe "deletable" (jc^.jcDeletable)
          , singleMaybe "name" (jc^.jcName)
          , single "tags" (jc^.jcTags)
          , singleMaybe "source_hidden" (jc^.jcSourceHidden)
          , singleMaybe "output_hidden" (jc^.jcOutputHidden)
          , singleMaybe "format" (jc^.jcFormat) ]
  where single k v = H.singleton k (toJSON v)
        singleMaybe _ Nothing = H.empty
        singleMaybe k (Just x) = single k x

instance ToJSON JupyterCell where
  toEncoding jc@CMarkdown { _jcAttachments } =
    pairs ("cell_type" .= asText "markdown"
           <> "source" .=  T.lines (jc^.jcSource)
           <> "metadata" .= cellMetadata jc
           <> "attachments" .= _jcAttachments)

  toEncoding jc@CCode { _jcExecutionCount } =
    pairs ("cell_type" .= asText "code"
           <> "source" .= T.lines (jc^.jcSource)
           <> "metadata" .= cellMetadata jc
           <> "execution_count" .= _jcExecutionCount
           <> "outputs" .= (jc^.jcOutputs))

  toEncoding jc@CRaw {} =
    pairs ("cell_type" .= asText "raw"
           <> "metadata" .= cellMetadata jc
           <> "source" .= T.lines (jc^.jcSource))

  toJSON jc@CMarkdown { _jcAttachments } =
    objectMaybe [ "cell_type" .= asText "markdown"
                , "source" .=  T.lines (jc^.jcSource)
                , "metadata" .= cellMetadata jc
                , "attachments" .= _jcAttachments ]

  toJSON jc@CCode { _jcExecutionCount } =
    objectMaybe [ "cell_type" .= asText "code"
                , "source" .= T.lines (jc^.jcSource)
                , "metadata" .= cellMetadata jc
                , "execution_count" .= _jcExecutionCount
                , "outputs" .= (jc^.jcOutputs) ]

  toJSON jc@CRaw {} =
    objectMaybe [ "cell_type" .= asText "raw"
                , "metadata" .= cellMetadata jc
                , "source" .= T.lines (jc^.jcSource) ]

instance ToJSON JupyterLanguage where
  toEncoding jl = pairs ( "name" .= (jl^.jlName)
                          <> "version" .=? (jl^.jlVersion)
                          <> "codemirror_mode" .=? (jl^.jlCodeMirrorMode))

  toJSON jl = objectMaybe [ "name" .= (jl^.jlName)
                          , "version" .=? (jl^.jlVersion)
                          , "codemirror_mode" .=? (jl^.jlCodeMirrorMode) ]

instance FromJSON JupyterLanguage where
  parseJSON = withObject "JupyterLanguage" $ \v -> JL
    <$> v .: "language"
    <*> v .:? "version"
    <*> v .:? "codemirror_mode"

instance ToJSON JupyterKernel where
  toEncoding jk = pairs ( "name" .= (jk^.jkName))
  toJSON jk = objectMaybe [ "name" .= (jk^.jkName) ]

instance FromJSON JupyterKernel where
  parseJSON = withObject "JupyterKernel" $ \v -> JK
    <$> v .: "name"

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
