{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Zeppelin where

import Data.Default (def)
import qualified Data.Default as D
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (mapAccumL)
import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
-- import qualified Text.Pandoc.Builder as P
import Control.Lens hiding ((.=))

import qualified Notebook as N

data ZeppelinNotebook = ZN { _znAngularObjects :: H.HashMap Text Value
                           , _znConfig         :: ZeppelinConfig
                           , _znParagraphs     :: [ZeppelinParagraph]
                           , _znName           :: Text
                           , _znId             :: Text
                           , _znInfo           :: ZeppelinInfo }
  deriving Show

instance D.Default ZeppelinNotebook where
  def = ZN H.empty def [] "" "" def

newtype ZeppelinConfig = ZC String
  deriving Show

instance D.Default ZeppelinConfig where
  def = ZC ""

newtype ZeppelinInfo = ZI ()
  deriving Show

instance D.Default ZeppelinInfo where
  def = ZI ()

data ZeppelinParagraph = ZP {_zpFocus                    :: Maybe Bool
                            ,_zpStatus                   :: Text
                            ,_zpApps                     :: [Value]
                            ,_zpConfig                   :: Value
                            ,_zpProgressUpdateIntervalMs :: Double
                            ,_zpSettings                 :: Value
                            ,_zpText                     :: Text
                            ,_zpJobName                  :: Text
                            ,_zpResult                   :: Maybe Value
                            ,_zpDateUpdated              :: Text
                            ,_zpDateCreated              :: Text
                            ,_zpDateStarted              :: Maybe Text
                            ,_zpDateFinished             :: Maybe Text
                            ,_zpHashKey                  :: Text
                            ,_zpId                       :: Text
                            ,_zpErrorMessage             :: Maybe Value }
  deriving Show

makeLenses ''ZeppelinNotebook
makeLenses ''ZeppelinParagraph

instance D.Default ZeppelinParagraph where
  def = ZP Nothing "" [] Null  0.0  Null "" "" Nothing "" "" Nothing Nothing "" "" Nothing

instance ToJSON ZeppelinParagraph where
  toEncoding zp = pairs ( "focus" .= (zp^.zpFocus)
                          <> "status" .= (zp^.zpStatus)
                          <> "apps" .= (zp^.zpApps)
                          <> "config" .= (zp^.zpConfig)
                          <> "progressUpdateIntervalMs"
                              .= (zp^.zpProgressUpdateIntervalMs)
                          <> "settings" .= (zp^.zpSettings)
                          <> "text" .= (zp^.zpText)
                          <> "jobName" .= (zp^.zpJobName)
                          <> "result" .= (zp^.zpResult)
                          <> "dateUpdated" .= (zp^.zpDateUpdated)
                          <> "dateCreated" .= (zp^.zpDateCreated)
                          <> "dateStarted" .= (zp^.zpDateStarted)
                          <> "dateFinished" .= (zp^.zpDateFinished)
                          <> "$$hashKey" .= (zp^.zpHashKey)
                          <> "id" .= (zp^.zpId)
                          <> "errorMessage" .= (zp^.zpErrorMessage) )

  toJSON zp = object [ "focus" .= (zp^.zpFocus)
                     , "status" .= (zp^.zpStatus)
                     , "apps" .= (zp^.zpApps)
                     , "config" .= (zp^.zpConfig)
                     , "progressUpdateIntervalMs"
                       .= (zp^.zpProgressUpdateIntervalMs)
                     , "settings" .= (zp^.zpSettings)
                     , "text" .= (zp^.zpText)
                     , "jobName" .= (zp^.zpJobName)
                     , "result" .= (zp^.zpResult)
                     , "dateUpdated" .= (zp^.zpDateUpdated)
                     , "dateCreated" .= (zp^.zpDateCreated)
                     , "dateStarted" .= (zp^.zpDateStarted)
                     , "dateFinished" .= (zp^.zpDateFinished)
                     , "$$hashKey" .= (zp^.zpHashKey)
                     , "id" .= (zp^.zpId)
                     , "errorMessage" .= (zp^.zpErrorMessage) ]

instance FromJSON ZeppelinParagraph where
  parseJSON = withObject "Paragraph" $ \v -> ZP
    <$> v .:? "focus"
    <*> v .:  "status"
    <*> v .:  "apps"
    <*> v .:  "config"
    <*> v .:  "progressUpdateIntervalMs"
    <*> v .:  "settings"
    <*> v .:  "text"
    <*> v .:  "jobName"
    <*> v .:? "result"
    <*> v .:  "dateUpdated"
    <*> v .:  "dateCreated"
    <*> v .:? "dateStarted"
    <*> v .:? "dateFinished"
    <*> v .:  "$$hashKey"
    <*> v .:  "id"
    <*> v .:? "errorMessage"

instance ToJSON ZeppelinNotebook where
  toEncoding zn = pairs ( "angularObjects" .= (zn^.znAngularObjects)
                          <> "config"      .= (zn^.znConfig)
                          <> "paragraphs"  .= (zn^.znParagraphs)
                          <> "name"        .= (zn^.znName)
                          <> "id"          .= (zn^.znId)
                          <> "info"        .= (zn^.znInfo))

  toJSON zn = object [ "angularObjects" .= (zn^.znAngularObjects)
                     , "config"      .= (zn^.znConfig)
                     , "paragraphs"  .= (zn^.znParagraphs)
                     , "name"        .= (zn^.znName)
                     , "id"          .= (zn^.znId)
                     , "info"        .= (zn^.znInfo) ]

instance FromJSON ZeppelinNotebook where
  parseJSON = withObject "ZeppelinNotebook" $ \v -> ZN
    <$> v .: "angularObjects"
    <*> v .: "config"
    <*> v .: "paragraphs"
    <*> v .: "name"
    <*> v .: "id"
    <*> v .: "info"

instance ToJSON ZeppelinConfig where
  toEncoding (ZC looknfeel) = pairs ("looknfeel" .= looknfeel)
  toJSON (ZC looknfeel) = object [ "looknfeel" .= looknfeel ]

instance FromJSON ZeppelinConfig where
  parseJSON = withObject "ZeppelinConfig" $ \v -> ZC <$> (v .: "looknfeel")

instance ToJSON ZeppelinInfo where
  toEncoding zi = pairs mempty
  toJSON zi = object []

instance FromJSON ZeppelinInfo where
  parseJSON = withObject "ZeppelinInfo" (const (return (ZI ())))

fromByteString :: B.ByteString -> Either String ZeppelinNotebook
fromByteString = eitherDecode

toByteString :: ZeppelinNotebook -> B.ByteString
toByteString = encode

fromNotebook :: N.Notebook -> ZeppelinNotebook
fromNotebook nb = runMeN def
  where runMeN = foldl1 (.) [ znName .~ (nb^.N.nName)
                            , znParagraphs .~ map toZParagraph (nb^.N.nCommands) ]
        toZParagraph nc = runMeP def
          where runMeP = case nc^.N.cLanguage of
                  Nothing -> zpText .~ (nc^.N.cCommand)
                  Just l  -> zpText .~ addLang l (nc^.N.cCommand)
                addLang l c = T.unlines [ T.cons '%' l, c ]

toNotebook :: ZeppelinNotebook -> N.Notebook
toNotebook zn = N.N (zn^.znName) (toCommands (zn^.znParagraphs))
  where toCommands = snd . mapAccumL toCommand Nothing
        toCommand :: Maybe Text -> ZeppelinParagraph -> (Maybe Text, N.Command)
        toCommand prev zp =
          let (langTag, rawCommand) = splitLangTag (zp^.zpText) in
          case langTag of
            Nothing -> (prev, N.C prev rawCommand)
            lang    -> (lang, N.C lang rawCommand)
        splitLangTag unparsedCommand =
          if unparsedCommand `T.index` 0 == '%'
          then let (x:xs) = T.lines unparsedCommand
               in (Just (T.stripEnd . T.tail $ x), T.unlines xs)
          else (Nothing, unparsedCommand)

-- toPandoc :: ZeppelinNotebook -> P.Pandoc
-- toPandoc z = P.doc $ foldMap (P.codeBlock . unpack . pText) (znParagraphs z)

-- fromPandoc :: P.Pandoc -> ZeppelinNotebook
-- fromPandoc = undefined
