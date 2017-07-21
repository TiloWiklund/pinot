{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Databricks where

import Data.Default (def)
import qualified Data.Default as D
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Monoid ((<>))
import Data.Traversable (mapAccumL)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import qualified Text.Pandoc.Builder as P
import Control.Lens hiding ((.=))

import qualified Notebook as N

-- jsonType :: Value -> String
-- jsonType (Object _) = "Object"
-- jsonType (Array _) = "Array"
-- jsonType (String _) = "String"
-- jsonType (Number _) = "Number"
-- jsonType (Bool _) = "Bool"
-- jsonType Null = "Null"

-- jsonKeys :: Value -> [Text]
-- jsonKeys (Object o) = H.keys o
-- jsonKeys _ = []

data DBNotebook = DBN { _dbnCommands     :: [DBCommand]
                      , _dbnDashboards   :: [Value]
                      , _dbnIPythonMeta  :: Value
                      , _dbnInputWidgets :: Value
                      , _dbnName         :: Text
                      , _dbnGuid         :: Text
                      , _dbnVersion      :: Value -- some sort of enum
                      , _dbnLanguage     :: Text
                      , _dbnGlobalVars   :: Value
                      , _dbnOrigID       :: Value } -- Integer?
  deriving Show

instance D.Default DBNotebook where
  def = DBN [] [] Null Null "" "" Null "" Null Null

data DBCommand = DBC { _dbcCustomPlotOptions :: Value
                     , _dbcErrorSummary      :: Value
                     , _dbcHeight            :: Value
                     , _dbcDiffDeletes       :: Value
                     , _dbcCommandTitle      :: Text
                     , _dbcState             :: Value
                     , _dbcCommand           :: Text
                     , _dbcResults           :: Value
                     , _dbcCommandVersion    :: Value
                     , _dbcXColumns          :: Value
                     , _dbcStartTime         :: Value
                     , _dbcIPythonMetadata   :: Value
                     , _dbcError             :: Value
                     , _dbcPivotAggregation  :: Value
                     , _dbcWidth             :: Value
                     , _dbcNuid              :: Text
                     , _dbcPivotColumns      :: Value
                     , _dbcInputWidgets      :: Value
                     , _dbcSubtype           :: Value
                     , _dbcYColumns          :: Value
                     , _dbcShowCommandTitle  :: Value
                     , _dbcGuid              :: Text
                     , _dbcCommandType       :: Value
                     , _dbcCollapsed         :: Value
                     , _dbcVersion           :: Value
                     , _dbcLatestUser        :: Value
                     , _dbcBindings          :: Value
                     , _dbcHideCommandCode   :: Bool
                     , _dbcDisplayType       :: Value
                     , _dbcGlobalVars        :: Value
                     , _dbcCommentThread     :: Value
                     , _dbcWorkflows         :: Value
                     , _dbcParentHierarchy   :: Value
                     , _dbcHideCommandResult :: Bool
                     , _dbcFinishTime        :: Value
                     , _dbcCommentsVisible   :: Value
                     , _dbcOrigId            :: Value
                     , _dbcSubmitTime        :: Value
                     , _dbcDiffInserts       :: Value
                     , _dbcPosition          :: Value }
  deriving Show

makeLenses ''DBNotebook
makeLenses ''DBCommand

instance D.Default DBCommand where
  def = DBC Null Null Null Null "" Null "" Null Null Null Null Null Null Null Null "" Null Null Null Null Null "" Null Null Null Null Null False Null Null Null Null Null False Null Null Null Null Null Null

instance ToJSON DBCommand where
  toJSON dbc = object [ "customPlotOptions" .= (dbc^.dbcCustomPlotOptions)
                      , "errorSummary" .= (dbc^.dbcErrorSummary)
                      , "height" .= (dbc^.dbcHeight)
                      , "diffDeletes" .= (dbc^.dbcDiffDeletes)
                      , "commandTitle" .= (dbc^.dbcCommandTitle)
                      , "state" .= (dbc^.dbcState)
                      , "command" .= (dbc^.dbcCommand)
                      , "results" .= (dbc^.dbcResults)
                      , "commandVersion" .= (dbc^.dbcCommandVersion)
                      , "xColumns" .= (dbc^.dbcXColumns)
                      , "startTime" .= (dbc^.dbcStartTime)
                      , "iPythonMetadata" .= (dbc^.dbcIPythonMetadata)
                      , "error" .= (dbc^.dbcError)
                      , "pivotAggregation" .= (dbc^.dbcPivotAggregation)
                      , "width" .= (dbc^.dbcWidth)
                      , "nuid" .= (dbc^.dbcNuid)
                      , "pivotColumns" .= (dbc^.dbcPivotColumns)
                      , "inputWidgets" .= (dbc^.dbcInputWidgets)
                      , "subtype" .= (dbc^.dbcSubtype)
                      , "yColumns" .= (dbc^.dbcYColumns)
                      , "showCommandTitle" .= (dbc^.dbcShowCommandTitle)
                      , "guid" .= (dbc^.dbcGuid)
                      , "commandType" .= (dbc^.dbcCommandType)
                      , "collapsed" .= (dbc^.dbcCollapsed)
                      , "version" .= (dbc^.dbcVersion)
                      , "latestUser" .= (dbc^.dbcLatestUser)
                      , "bindings" .= (dbc^.dbcBindings)
                      , "hideCommandCode" .= (dbc^.dbcHideCommandCode)
                      , "displayType" .= (dbc^.dbcDisplayType)
                      , "globalVars" .= (dbc^.dbcGlobalVars)
                      , "commentThread" .= (dbc^.dbcCommentThread)
                      , "workflows" .= (dbc^.dbcWorkflows)
                      , "parentHierarchy" .= (dbc^.dbcParentHierarchy)
                      , "hideCommandResult" .= (dbc^.dbcHideCommandResult)
                      , "finishTime" .= (dbc^.dbcFinishTime)
                      , "commentsVisible" .= (dbc^.dbcCommentsVisible)
                      , "origId" .= (dbc^.dbcOrigId)
                      , "submitTime" .= (dbc^.dbcSubmitTime)
                      , "diffInserts" .= (dbc^.dbcDiffInserts)
                      , "position" .= (dbc^.dbcPosition) ]

  toEncoding dbc = pairs ( "customPlotOptions" .= (dbc^.dbcCustomPlotOptions)
                           <> "errorSummary" .= (dbc^.dbcErrorSummary)
                           <> "height" .= (dbc^.dbcHeight)
                           <> "diffDeletes" .= (dbc^.dbcDiffDeletes)
                           <> "commandTitle" .= (dbc^.dbcCommandTitle)
                           <> "state" .= (dbc^.dbcState)
                           <> "command" .= (dbc^.dbcCommand)
                           <> "results" .= (dbc^.dbcResults)
                           <> "commandVersion" .= (dbc^.dbcCommandVersion)
                           <> "xColumns" .= (dbc^.dbcXColumns)
                           <> "startTime" .= (dbc^.dbcStartTime)
                           <> "iPythonMetadata" .= (dbc^.dbcIPythonMetadata)
                           <> "error" .= (dbc^.dbcError)
                           <> "pivotAggregation" .= (dbc^.dbcPivotAggregation)
                           <> "width" .= (dbc^.dbcWidth)
                           <> "nuid" .= (dbc^.dbcNuid)
                           <> "pivotColumns" .= (dbc^.dbcPivotColumns)
                           <> "inputWidgets" .= (dbc^.dbcInputWidgets)
                           <> "subtype" .= (dbc^.dbcSubtype)
                           <> "yColumns" .= (dbc^.dbcYColumns)
                           <> "showCommandTitle" .= (dbc^.dbcShowCommandTitle)
                           <> "guid" .= (dbc^.dbcGuid)
                           <> "commandType" .= (dbc^.dbcCommandType)
                           <> "collapsed" .= (dbc^.dbcCollapsed)
                           <> "version" .= (dbc^.dbcVersion)
                           <> "latestUser" .= (dbc^.dbcLatestUser)
                           <> "bindings" .= (dbc^.dbcBindings)
                           <> "hideCommandCode" .= (dbc^.dbcHideCommandCode)
                           <> "displayType" .= (dbc^.dbcDisplayType)
                           <> "globalVars" .= (dbc^.dbcGlobalVars)
                           <> "commentThread" .= (dbc^.dbcCommentThread)
                           <> "workflows" .= (dbc^.dbcWorkflows)
                           <> "parentHierarchy" .= (dbc^.dbcParentHierarchy)
                           <> "hideCommandResult" .= (dbc^.dbcHideCommandResult)
                           <> "finishTime" .= (dbc^.dbcFinishTime)
                           <> "commentsVisible" .= (dbc^.dbcCommentsVisible)
                           <> "origId" .= (dbc^.dbcOrigId)
                           <> "submitTime" .= (dbc^.dbcSubmitTime)
                           <> "diffInserts" .= (dbc^.dbcDiffInserts)
                           <> "position" .= (dbc^.dbcPosition) )

instance FromJSON DBCommand where
  parseJSON = withObject "DBCommand" $ \v -> DBC
    <$> v .: "customPlotOptions"
    <*> v .: "errorSummary"
    <*> v .: "height"
    <*> v .: "diffDeletes"
    <*> v .: "commandTitle"
    <*> v .: "state"
    <*> v .: "command"
    <*> v .: "results"
    <*> v .: "commandVersion"
    <*> v .: "xColumns"
    <*> v .: "startTime"
    <*> v .: "iPythonMetadata"
    <*> v .: "error"
    <*> v .: "pivotAggregation"
    <*> v .: "width"
    <*> v .: "nuid"
    <*> v .: "pivotColumns"
    <*> v .: "inputWidgets"
    <*> v .: "subtype"
    <*> v .: "yColumns"
    <*> v .: "showCommandTitle"
    <*> v .: "guid"
    <*> v .: "commandType"
    <*> v .: "collapsed"
    <*> v .: "version"
    <*> v .: "latestUser"
    <*> v .: "bindings"
    <*> v .: "hideCommandCode"
    <*> v .: "displayType"
    <*> v .: "globalVars"
    <*> v .: "commentThread"
    <*> v .: "workflows"
    <*> v .: "parentHierarchy"
    <*> v .: "hideCommandResult"
    <*> v .: "finishTime"
    <*> v .: "commentsVisible"
    <*> v .: "origId"
    <*> v .: "submitTime"
    <*> v .: "diffInserts"
    <*> v .: "position"

instance ToJSON DBNotebook where
  toJSON dbn = object [ "commands" .= (dbn^.dbnCommands)
                      , "dashboards" .= (dbn^.dbnDashboards)
                      , "iPythonMetadata" .= (dbn^.dbnIPythonMeta)
                      , "inputWidgets" .= (dbn^.dbnInputWidgets)
                      , "name" .= (dbn^.dbnName)
                      , "guid" .= (dbn^.dbnGuid)
                      , "version" .= (dbn^.dbnVersion)
                      , "language" .= (dbn^.dbnLanguage)
                      , "globalVars" .= (dbn^.dbnGlobalVars)
                      , "origId" .= (dbn^.dbnOrigID) ]

  toEncoding dbn = pairs ( "commands" .= (dbn^.dbnCommands)
                         <> "dashboards" .= (dbn^.dbnDashboards)
                         <> "iPythonMetadata" .= (dbn^.dbnIPythonMeta)
                         <> "inputWidgets" .= (dbn^.dbnInputWidgets)
                         <> "name" .= (dbn^.dbnName)
                         <> "guid" .= (dbn^.dbnGuid)
                         <> "version" .= (dbn^.dbnVersion)
                         <> "language" .= (dbn^.dbnLanguage)
                         <> "globalVars" .= (dbn^.dbnGlobalVars)
                         <> "origId" .= (dbn^.dbnOrigID) )

instance FromJSON DBNotebook where
  parseJSON = withObject "DBNotebook" $ \v -> DBN
    <$> v .: "commands"
    <*> v .: "dashboards"
    <*> v .: "iPythonMetadata"
    <*> v .: "inputWidgets"
    <*> v .: "name"
    <*> v .: "guid"
    <*> v .: "version"
    <*> v .: "language"
    <*> v .: "globalVars"
    <*> v .: "origId"

fromByteString :: B.ByteString -> Either String DBNotebook
fromByteString = eitherDecode

toByteString :: DBNotebook -> B.ByteString
toByteString = encode

toNotebook :: DBNotebook -> N.Notebook
toNotebook db = N.N (db^.dbnName) (toCommands (db^.dbnCommands))
  where toCommands = snd . mapAccumL toCommand Nothing
        toCommand :: Maybe Text -> DBCommand -> (Maybe Text, N.Command)
        toCommand prev dbc =
          let (langTag, rawCommand) = splitLangTag (dbc^.dbcCommand) in
          case langTag of
            Nothing -> (prev, N.C prev rawCommand)
            lang    -> (lang, N.C lang rawCommand)
        splitLangTag unparsedCommand =
          if unparsedCommand `T.index` 0 == '%'
          then let (x:xs) = T.lines unparsedCommand
               in (Just (T.stripEnd . T.tail $ x), T.unlines xs)
          else (Nothing, unparsedCommand)

fromNotebook :: N.Notebook -> DBNotebook
fromNotebook nb = runMeN def
  where runMeN = foldl1 (.) [ dbnName .~ (nb^.N.nName)
                            , dbnCommands .~ map toNBCommand (nb^.N.nCommands) ]
        toNBCommand nc = runMeC def
          where runMeC = case nc^.N.cLanguage of
                  Nothing -> dbcCommand .~ (nc^.N.cCommand)
                  Just l  -> dbcCommand .~ addLang l (nc^.N.cCommand)
                addLang l c = T.unlines [ T.cons '%' l, c ]

-- main :: IO ()
-- main = do
--   --[p] <- getArgs
--   -- let p = "data/20160826_KDL_Intro2BDA_HumSocSci/00_workshop_Overview.scala"
--   -- let p = "data/20160826_KDL_Intro2BDA_HumSocSci/01_introduction/002_loginToDatabricksCE.scala"
--   let p = "data/scalable-data-science/week5/10_LinearRegressionIntro/018_LinRegIntro.scala"
--   f <- B.readFile p
--   let dbn = either error id (eitherDecode f :: Either String DBNotebook)
--   -- print (jsonType $ dbcCommand (dbnCommands dbn !! 0))
--   -- print (jsonKeys $ dbcCommand (dbnCommands dbn !! 0))
--   -- print (dbnCommands dbn !! 0)
--   print (jsonType $ dbnOrigID dbn)
--   print (jsonKeys $ dbnOrigID dbn)
--   print (dbnOrigID dbn)
--   return ()
