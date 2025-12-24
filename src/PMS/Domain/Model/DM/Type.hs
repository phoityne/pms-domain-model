{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Model.DM.Type where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Text.Read as R
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import System.Exit
import System.IO 

import PMS.Domain.Model.DM.TH


--------------------------------------------------------------------------------
-- |
--
instance FromJSON LogLevel where
  parseJSON (String v) = case R.readEither ("Level" ++ T.unpack v) of
    Right l -> pure l
    Left er -> error $ "invalid loglevel. <" ++ T.unpack v ++ "> " ++ er
  parseJSON o = error $ "json parse error. Priority:" ++ show o

instance ToJSON LogLevel where
  toJSON  LevelDebug    = String $ T.pack "Debug"
  toJSON  LevelInfo     = String $ T.pack "Info"
  toJSON  LevelWarn     = String $ T.pack "Warn"
  toJSON  LevelError    = String $ T.pack "Error"
  toJSON (LevelOther m) = String m

--------------------------------------------------------------------------------
-- |
--
newtype RawJsonString = RawJsonString { unRawJsonString :: String }
  deriving (Show, Read, Eq)

instance FromJSON RawJsonString where
  parseJSON = pure . RawJsonString . BL.unpack . encode

instance ToJSON RawJsonString where
  toJSON (RawJsonString str) =
    case eitherDecode (BL.pack str) of
      Right v -> v
      Left  e -> error $ show e

instance Default RawJsonString where
  def = RawJsonString ""

-- |
--
newtype RawJsonByteString = RawJsonByteString { unRawJsonByteString :: BL.ByteString }
  deriving (Show, Read, Eq)

instance FromJSON RawJsonByteString where
  parseJSON = pure . RawJsonByteString . encode

instance ToJSON RawJsonByteString where
  toJSON (RawJsonByteString bs) =
    case eitherDecode bs of
      Right v -> v
      Left  e -> error $ show e

instance Default RawJsonByteString where
  def = RawJsonByteString ""

--------------------------------------------------------------------------------
-- |
--
data JsonRpcRequest =
  JsonRpcRequest {
    _jsonrpcJsonRpcRequest  :: String
  , _idJsonRpcRequest       :: Maybe Int
  , _methodJsonRpcRequest   :: String
  , _paramsJsonRpcRequest   :: Maybe RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "JsonRpcRequest", omitNothingFields = True} ''JsonRpcRequest)
makeLenses ''JsonRpcRequest

instance Default JsonRpcRequest where
  def = JsonRpcRequest {
        _jsonrpcJsonRpcRequest  = def
      , _idJsonRpcRequest  = def
      , _methodJsonRpcRequest = def
      , _paramsJsonRpcRequest = def
      }

-- |
--
data JsonRpcResponse =
  JsonRpcResponse {
    _jsonrpcJsonRpcResponse :: String
  , _idJsonRpcResponse      :: Int
  , _resultJsonRpcResponse  :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "JsonRpcResponse", omitNothingFields = True} ''JsonRpcResponse)
makeLenses ''JsonRpcResponse

instance Default JsonRpcResponse where
  def = JsonRpcResponse {
        _jsonrpcJsonRpcResponse = ""
      , _idJsonRpcResponse = 0
      , _resultJsonRpcResponse = RawJsonByteString ""
      }

defaultJsonRpcResponse :: JsonRpcRequest -> JsonRpcResponse
defaultJsonRpcResponse req = def {
    _jsonrpcJsonRpcResponse = req^.jsonrpcJsonRpcRequest
  , _idJsonRpcResponse = maybe 0 id (req^.idJsonRpcRequest)
  , _resultJsonRpcResponse = def
  }

-- |
--
data JsonRpcNotification =
  JsonRpcNotification {
    _jsonrpcJsonRpcNotification :: String
  , _methodJsonRpcNotification  :: String
  , _paramsJsonRpcNotification  :: Maybe RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "JsonRpcNotification", omitNothingFields = True} ''JsonRpcNotification)
makeLenses ''JsonRpcNotification

instance Default JsonRpcNotification where
  def = JsonRpcNotification {
        _jsonrpcJsonRpcNotification = "2.0"
      , _methodJsonRpcNotification  = def
      , _paramsJsonRpcNotification  = def
      }

--------------------------------------------------------------------------------

-- |
--
data McpInitializeRequestParams =
  McpInitializeRequestParams {
    _protocolVersionMcpInitializeRequestParams :: String
  , _capabilitiesMcpInitializeRequestParams    :: RawJsonByteString
  , _clientInfoMcpInitializeRequestParams      :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeRequestParams", omitNothingFields = True} ''McpInitializeRequestParams)
makeLenses ''McpInitializeRequestParams

instance Default McpInitializeRequestParams where
  def = McpInitializeRequestParams {
        _protocolVersionMcpInitializeRequestParams = def
      , _capabilitiesMcpInitializeRequestParams = def
      , _clientInfoMcpInitializeRequestParams = def
      }

-- |
--
data McpInitializeRequestData =
  McpInitializeRequestData {
    _jsonrpcMcpInitializeRequestData :: JsonRpcRequest
  , _paramsMcpInitializeRequestData  :: McpInitializeRequestParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeRequestData", omitNothingFields = True} ''McpInitializeRequestData)
makeLenses ''McpInitializeRequestData

instance Default McpInitializeRequestData where
  def = McpInitializeRequestData {
        _jsonrpcMcpInitializeRequestData = def
      , _paramsMcpInitializeRequestData = def
      }

-- |
--
data McpToolsListRequestData =
  McpToolsListRequestData {
    _jsonrpcMcpToolsListRequestData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListRequestData", omitNothingFields = True} ''McpToolsListRequestData)
makeLenses ''McpToolsListRequestData

instance Default McpToolsListRequestData where
  def = McpToolsListRequestData {
        _jsonrpcMcpToolsListRequestData = def
      }

-- |
--
data McpToolsCallRequestDataParams =
  McpToolsCallRequestDataParams {
    _nameMcpToolsCallRequestDataParams :: String
  , _argumentsMcpToolsCallRequestDataParams :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallRequestDataParams", omitNothingFields = True} ''McpToolsCallRequestDataParams)
makeLenses ''McpToolsCallRequestDataParams

instance Default McpToolsCallRequestDataParams where
  def = McpToolsCallRequestDataParams {
        _nameMcpToolsCallRequestDataParams = def
      , _argumentsMcpToolsCallRequestDataParams  = def
      }


-- |
--
data McpToolsCallRequestData =
  McpToolsCallRequestData {
    _jsonrpcMcpToolsCallRequestData :: JsonRpcRequest
  , _paramsMcpToolsCallRequestData  :: McpToolsCallRequestDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallRequestData", omitNothingFields = True} ''McpToolsCallRequestData)
makeLenses ''McpToolsCallRequestData

instance Default McpToolsCallRequestData where
  def = McpToolsCallRequestData {
        _jsonrpcMcpToolsCallRequestData = def
      , _paramsMcpToolsCallRequestData  = def
      }


-- |
--
data McpPromptsListRequestData =
  McpPromptsListRequestData {
    _jsonrpcMcpPromptsListRequestData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsListRequestData", omitNothingFields = True} ''McpPromptsListRequestData)
makeLenses ''McpPromptsListRequestData

instance Default McpPromptsListRequestData where
  def = McpPromptsListRequestData {
        _jsonrpcMcpPromptsListRequestData = def
      }

-- |
--
data McpPromptsGetRequestDataParams =
  McpPromptsGetRequestDataParams {
    _nameMcpPromptsGetRequestDataParams :: String
  , _argumentsMcpPromptsGetRequestDataParams  :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsGetRequestDataParams", omitNothingFields = True} ''McpPromptsGetRequestDataParams)
makeLenses ''McpPromptsGetRequestDataParams

instance Default McpPromptsGetRequestDataParams where
  def = McpPromptsGetRequestDataParams {
        _nameMcpPromptsGetRequestDataParams = def
      , _argumentsMcpPromptsGetRequestDataParams  = def
      }

-- |
--
data McpPromptsGetRequestData =
  McpPromptsGetRequestData {
    _jsonrpcMcpPromptsGetRequestData :: JsonRpcRequest
  , _paramsMcpPromptsGetRequestData  :: McpPromptsGetRequestDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsGetRequestData", omitNothingFields = True} ''McpPromptsGetRequestData)
makeLenses ''McpPromptsGetRequestData

instance Default McpPromptsGetRequestData where
  def = McpPromptsGetRequestData {
        _jsonrpcMcpPromptsGetRequestData = def
      , _paramsMcpPromptsGetRequestData  = def
      }





-- |
--
data McpResourcesTemplatesListRequestData =
  McpResourcesTemplatesListRequestData {
    _jsonrpcMcpResourcesTemplatesListRequestData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesTemplatesListRequestData", omitNothingFields = True} ''McpResourcesTemplatesListRequestData)
makeLenses ''McpResourcesTemplatesListRequestData

instance Default McpResourcesTemplatesListRequestData where
  def = McpResourcesTemplatesListRequestData {
        _jsonrpcMcpResourcesTemplatesListRequestData = def
      }


-- |
--
data McpResourcesListRequestData =
  McpResourcesListRequestData {
    _jsonrpcMcpResourcesListRequestData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesListRequestData", omitNothingFields = True} ''McpResourcesListRequestData)
makeLenses ''McpResourcesListRequestData

instance Default McpResourcesListRequestData where
  def = McpResourcesListRequestData {
        _jsonrpcMcpResourcesListRequestData = def
      }

-- |
--
data McpResourcesReadRequestDataParams =
  McpResourcesReadRequestDataParams {
    _uriMcpResourcesReadRequestDataParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesReadRequestDataParams", omitNothingFields = True} ''McpResourcesReadRequestDataParams)
makeLenses ''McpResourcesReadRequestDataParams

instance Default McpResourcesReadRequestDataParams where
  def = McpResourcesReadRequestDataParams {
        _uriMcpResourcesReadRequestDataParams = def
      }

-- |
--
data McpResourcesReadRequestData =
  McpResourcesReadRequestData {
    _jsonrpcMcpResourcesReadRequestData :: JsonRpcRequest
  , _paramsMcpResourcesReadRequestData  :: McpResourcesReadRequestDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesReadRequestData", omitNothingFields = True} ''McpResourcesReadRequestData)
makeLenses ''McpResourcesReadRequestData

instance Default McpResourcesReadRequestData where
  def = McpResourcesReadRequestData {
        _jsonrpcMcpResourcesReadRequestData = def
      , _paramsMcpResourcesReadRequestData  = def
      }




-- |
--
data McpInitializedNotificationData =
  McpInitializedNotificationData {
    _jsonrpcMcpInitializedNotificationData :: JsonRpcRequest
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializedNotificationData", omitNothingFields = True} ''McpInitializedNotificationData)
makeLenses ''McpInitializedNotificationData

instance Default McpInitializedNotificationData where
  def = McpInitializedNotificationData {
        _jsonrpcMcpInitializedNotificationData = def
      }


-- |
--
data McpCancelledNotificationDataParams =
  McpCancelledNotificationDataParams {
    _requestIdMcpCancelledNotificationDataParams :: Int
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCancelledNotificationDataParams", omitNothingFields = True} ''McpCancelledNotificationDataParams)
makeLenses ''McpCancelledNotificationDataParams

instance Default McpCancelledNotificationDataParams where
  def = McpCancelledNotificationDataParams {
        _requestIdMcpCancelledNotificationDataParams = def
      }

-- |
--
data McpCancelledNotificationData =
  McpCancelledNotificationData {
    _jsonrpcMcpCancelledNotificationData :: JsonRpcRequest
  , _paramsMcpCancelledNotificationData  :: McpCancelledNotificationDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCancelledNotificationData", omitNothingFields = True} ''McpCancelledNotificationData)
makeLenses ''McpCancelledNotificationData

instance Default McpCancelledNotificationData where
  def = McpCancelledNotificationData {
        _jsonrpcMcpCancelledNotificationData = def
      , _paramsMcpCancelledNotificationData  = def
      }



-- |
--
data McpCompletionCompleteRequestDataParams =
  McpCompletionCompleteRequestDataParams {
    _refMcpCompletionCompleteRequestDataParams :: RawJsonByteString
  , _argumentMcpCompletionCompleteRequestDataParams  :: RawJsonByteString
  , _contextMcpCompletionCompleteRequestDataParams  :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCompletionCompleteRequestDataParams", omitNothingFields = True} ''McpCompletionCompleteRequestDataParams)
makeLenses ''McpCompletionCompleteRequestDataParams

instance Default McpCompletionCompleteRequestDataParams where
  def = McpCompletionCompleteRequestDataParams {
        _refMcpCompletionCompleteRequestDataParams = def
      , _argumentMcpCompletionCompleteRequestDataParams  = def
      , _contextMcpCompletionCompleteRequestDataParams  = def
      }

-- |
--
data McpCompletionCompleteRequestData =
  McpCompletionCompleteRequestData {
    _jsonrpcMcpCompletionCompleteRequestData :: JsonRpcRequest
  , _paramsMcpCompletionCompleteRequestData  :: McpCompletionCompleteRequestDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCompletionCompleteRequestData", omitNothingFields = True} ''McpCompletionCompleteRequestData)
makeLenses ''McpCompletionCompleteRequestData

instance Default McpCompletionCompleteRequestData where
  def = McpCompletionCompleteRequestData {
        _jsonrpcMcpCompletionCompleteRequestData = def
      , _paramsMcpCompletionCompleteRequestData  = def
      }


-- |
--
data McpRequest =
    McpInitializeRequest McpInitializeRequestData
  | McpToolsListRequest McpToolsListRequestData
  | McpToolsCallRequest McpToolsCallRequestData
  | McpPromptsListRequest McpPromptsListRequestData
  | McpPromptsGetRequest McpPromptsGetRequestData
  | McpResourcesTemplatesListRequest McpResourcesTemplatesListRequestData
  | McpResourcesListRequest McpResourcesListRequestData
  | McpResourcesReadRequest McpResourcesReadRequestData
  | McpInitializedNotification McpInitializedNotificationData
  | McpCancelledNotification McpCancelledNotificationData
  | McpCompletionCompleteRequest McpCompletionCompleteRequestData
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- |
--
data McpInitializeResponseResultCapabilitiesResources =
  McpInitializeResponseResultCapabilitiesResources {
    _subscribedMcpInitializeResponseResultCapabilitiesResources :: Bool
  , _listChangedMcpInitializeResponseResultCapabilitiesResources :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesResources", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesResources)
makeLenses ''McpInitializeResponseResultCapabilitiesResources

instance Default McpInitializeResponseResultCapabilitiesResources where
  def = McpInitializeResponseResultCapabilitiesResources {
        _subscribedMcpInitializeResponseResultCapabilitiesResources = False
      , _listChangedMcpInitializeResponseResultCapabilitiesResources = True
      }


data McpInitializeResponseResultCapabilitiesPrompts =
  McpInitializeResponseResultCapabilitiesPrompts {
    _listChangedMcpInitializeResponseResultCapabilitiesPrompts :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesPrompts", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesPrompts)
makeLenses ''McpInitializeResponseResultCapabilitiesPrompts

instance Default McpInitializeResponseResultCapabilitiesPrompts where
  def = McpInitializeResponseResultCapabilitiesPrompts {
        _listChangedMcpInitializeResponseResultCapabilitiesPrompts = True
      }

-- |
--
data McpInitializeResponseResultCapabilitiesTools =
  McpInitializeResponseResultCapabilitiesTools {
    _listChangedMcpInitializeResponseResultCapabilitiesTools :: Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilitiesTools", omitNothingFields = True} ''McpInitializeResponseResultCapabilitiesTools)
makeLenses ''McpInitializeResponseResultCapabilitiesTools

instance Default McpInitializeResponseResultCapabilitiesTools where
  def = McpInitializeResponseResultCapabilitiesTools {
        _listChangedMcpInitializeResponseResultCapabilitiesTools = True
      }

-- |
--
data McpInitializeResponseResultCapabilities =
  McpInitializeResponseResultCapabilities {
    _toolsMcpInitializeResponseResultCapabilities :: McpInitializeResponseResultCapabilitiesTools
  , _promptsMcpInitializeResponseResultCapabilities :: McpInitializeResponseResultCapabilitiesPrompts
  , _resourcesMcpInitializeResponseResultCapabilities :: McpInitializeResponseResultCapabilitiesResources
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultCapabilities", omitNothingFields = True} ''McpInitializeResponseResultCapabilities)
makeLenses ''McpInitializeResponseResultCapabilities

instance Default McpInitializeResponseResultCapabilities where
  def = McpInitializeResponseResultCapabilities {
        _toolsMcpInitializeResponseResultCapabilities = def
      , _promptsMcpInitializeResponseResultCapabilities = def
      , _resourcesMcpInitializeResponseResultCapabilities = def
      }

-- |
--
data McpInitializeResponseResultServerInfo =
  McpInitializeResponseResultServerInfo {
    _nameMcpInitializeResponseResultServerInfo    :: String
  , _versionMcpInitializeResponseResultServerInfo :: String
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResultServerInfo", omitNothingFields = True} ''McpInitializeResponseResultServerInfo)
makeLenses ''McpInitializeResponseResultServerInfo

instance Default McpInitializeResponseResultServerInfo where
  def = McpInitializeResponseResultServerInfo {
        _nameMcpInitializeResponseResultServerInfo = "pty-mcp-server"
      , _versionMcpInitializeResponseResultServerInfo = "0.0.4"
      }

-- |
--
data McpInitializeResponseResult =
  McpInitializeResponseResult {
    _protocolVersionMcpInitializeResponseResult :: String
  , _capabilitiesMcpInitializeResponseResult    :: McpInitializeResponseResultCapabilities
  , _serverInfoMcpInitializeResponseResult      :: McpInitializeResponseResultServerInfo
  , _instructionsMcpInitializeResponseResult    :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseResult", omitNothingFields = True} ''McpInitializeResponseResult)
makeLenses ''McpInitializeResponseResult

instance Default McpInitializeResponseResult where
  def = McpInitializeResponseResult {
        _protocolVersionMcpInitializeResponseResult = "2024-11-05"
      , _capabilitiesMcpInitializeResponseResult = def
      , _serverInfoMcpInitializeResponseResult = def
      , _instructionsMcpInitializeResponseResult = def
      }


-- |
--
data McpInitializeResponseData =
  McpInitializeResponseData {
    _jsonrpcMcpInitializeResponseData :: JsonRpcRequest
  , _resultMcpInitializeResponseData  :: McpInitializeResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpInitializeResponseData", omitNothingFields = True} ''McpInitializeResponseData)
makeLenses ''McpInitializeResponseData

instance Default McpInitializeResponseData where
  def = McpInitializeResponseData {
        _jsonrpcMcpInitializeResponseData = def
      , _resultMcpInitializeResponseData = def
      }


-- |
--
data McpToolsListResponseResult =
  McpToolsListResponseResult {
    _toolsMcpToolsListResponseResult :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListResponseResult", omitNothingFields = True} ''McpToolsListResponseResult)
makeLenses ''McpToolsListResponseResult

instance Default McpToolsListResponseResult where
  def = McpToolsListResponseResult {
        _toolsMcpToolsListResponseResult = def
      }

-- |
--
data McpToolsListResponseData =
  McpToolsListResponseData {
    _jsonrpcMcpToolsListResponseData :: JsonRpcRequest
  , _resultMcpToolsListResponseData  :: McpToolsListResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListResponseData", omitNothingFields = True} ''McpToolsListResponseData)
makeLenses ''McpToolsListResponseData

instance Default McpToolsListResponseData where
  def = McpToolsListResponseData {
        _jsonrpcMcpToolsListResponseData = def
      , _resultMcpToolsListResponseData = def
      }


-- |
--
data McpToolsCallResponseResultContent =
  McpToolsCallResponseResultContent {
    _typeMcpToolsCallResponseResultContent :: String
  , _textMcpToolsCallResponseResultContent :: String
  }
  | 
  McpToolsCallResponseResultImageContent {
    _typeMcpToolsCallResponseResultContent :: String
  , _dataMcpToolsCallResponseResultContent :: String
  , _mimeTypeToolsCallResponseResultContent :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseResultContent", omitNothingFields = True} ''McpToolsCallResponseResultContent)
makeLenses ''McpToolsCallResponseResultContent

instance Default McpToolsCallResponseResultContent where
  def = McpToolsCallResponseResultContent {
        _typeMcpToolsCallResponseResultContent = def
      , _textMcpToolsCallResponseResultContent = def
      }


-- |
--
data McpToolsCallResponseResult =
  McpToolsCallResponseResult {
    _contentMcpToolsCallResponseResult :: [McpToolsCallResponseResultContent]
  , _isErrorMcpToolsCallResponseResult :: Bool
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseResult", omitNothingFields = True} ''McpToolsCallResponseResult)
makeLenses ''McpToolsCallResponseResult

instance Default McpToolsCallResponseResult where
  def = McpToolsCallResponseResult {
        _contentMcpToolsCallResponseResult = def
      , _isErrorMcpToolsCallResponseResult = False
      }


-- |
--
data McpToolsCallResponseData =
  McpToolsCallResponseData {
    _jsonrpcMcpToolsCallResponseData :: JsonRpcRequest
  , _resultMcpToolsCallResponseData  :: McpToolsCallResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsCallResponseData", omitNothingFields = True} ''McpToolsCallResponseData)
makeLenses ''McpToolsCallResponseData

instance Default McpToolsCallResponseData where
  def = McpToolsCallResponseData {
        _jsonrpcMcpToolsCallResponseData = def
      , _resultMcpToolsCallResponseData = def
      }




-- |
--
data McpPromptsListResponseResult =
  McpPromptsListResponseResult {
    _promptsMcpPromptsListResponseResult :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsListResponseResult", omitNothingFields = True} ''McpPromptsListResponseResult)
makeLenses ''McpPromptsListResponseResult

instance Default McpPromptsListResponseResult where
  def = McpPromptsListResponseResult {
        _promptsMcpPromptsListResponseResult = def
      }

-- |
--
data McpPromptsListResponseData =
  McpPromptsListResponseData {
    _jsonrpcMcpPromptsListResponseData :: JsonRpcRequest
  , _resultMcpPromptsListResponseData  :: McpPromptsListResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsListResponseData", omitNothingFields = True} ''McpPromptsListResponseData)
makeLenses ''McpPromptsListResponseData

instance Default McpPromptsListResponseData where
  def = McpPromptsListResponseData {
        _jsonrpcMcpPromptsListResponseData = def
      , _resultMcpPromptsListResponseData = def
      }

-- |
--
data McpPromptsGetResponseResultPromptMessage =
  McpPromptsGetResponseResultPromptMessage {
    _roleMcpPromptsGetResponseResultPromptMessage :: String
  , _contentMcpPromptsGetResponseResultPromptMessage :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsGetResponseResultPromptMessage", omitNothingFields = True} ''McpPromptsGetResponseResultPromptMessage)
makeLenses ''McpPromptsGetResponseResultPromptMessage

instance Default McpPromptsGetResponseResultPromptMessage where
  def = McpPromptsGetResponseResultPromptMessage {
        _roleMcpPromptsGetResponseResultPromptMessage = "assistant"
      , _contentMcpPromptsGetResponseResultPromptMessage = def
      }

-- |
--
data McpPromptsGetResponseResult =
  McpPromptsGetResponseResult {
    _messagesMcpPromptsGetResponseResult :: [McpPromptsGetResponseResultPromptMessage]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsGetResponseResult", omitNothingFields = True} ''McpPromptsGetResponseResult)
makeLenses ''McpPromptsGetResponseResult

instance Default McpPromptsGetResponseResult where
  def = McpPromptsGetResponseResult {
        _messagesMcpPromptsGetResponseResult = def
      }

-- |
--
data McpPromptsGetResponseData =
  McpPromptsGetResponseData {
    _jsonrpcMcpPromptsGetResponseData :: JsonRpcRequest
  , _resultMcpPromptsGetResponseData  :: McpPromptsGetResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsGetResponseData", omitNothingFields = True} ''McpPromptsGetResponseData)
makeLenses ''McpPromptsGetResponseData

instance Default McpPromptsGetResponseData where
  def = McpPromptsGetResponseData {
        _jsonrpcMcpPromptsGetResponseData = def
      , _resultMcpPromptsGetResponseData = def
      }





-- |
--
data McpResourcesTemplatesListResponseResult =
  McpResourcesTemplatesListResponseResult {
    _resourceTemplatesMcpResourcesTemplatesListResponseResult :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesTemplatesListResponseResult", omitNothingFields = True} ''McpResourcesTemplatesListResponseResult)
makeLenses ''McpResourcesTemplatesListResponseResult

instance Default McpResourcesTemplatesListResponseResult where
  def = McpResourcesTemplatesListResponseResult {
        _resourceTemplatesMcpResourcesTemplatesListResponseResult = def
      }

-- |
--
data McpResourcesTemplatesListResponseData =
  McpResourcesTemplatesListResponseData {
    _jsonrpcMcpResourcesTemplatesListResponseData :: JsonRpcRequest
  , _resultMcpResourcesTemplatesListResponseData  :: McpResourcesTemplatesListResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesTemplatesListResponseData", omitNothingFields = True} ''McpResourcesTemplatesListResponseData)
makeLenses ''McpResourcesTemplatesListResponseData

instance Default McpResourcesTemplatesListResponseData where
  def = McpResourcesTemplatesListResponseData {
        _jsonrpcMcpResourcesTemplatesListResponseData = def
      , _resultMcpResourcesTemplatesListResponseData = def
      }



-- |
--
data McpResourcesListResponseResult =
  McpResourcesListResponseResult {
    _resourcesMcpResourcesListResponseResult :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesListResponseResult", omitNothingFields = True} ''McpResourcesListResponseResult)
makeLenses ''McpResourcesListResponseResult

instance Default McpResourcesListResponseResult where
  def = McpResourcesListResponseResult {
        _resourcesMcpResourcesListResponseResult = def
      }

-- |
--
data McpResourcesListResponseData =
  McpResourcesListResponseData {
    _jsonrpcMcpResourcesListResponseData :: JsonRpcRequest
  , _resultMcpResourcesListResponseData  :: McpResourcesListResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesListResponseData", omitNothingFields = True} ''McpResourcesListResponseData)
makeLenses ''McpResourcesListResponseData

instance Default McpResourcesListResponseData where
  def = McpResourcesListResponseData {
        _jsonrpcMcpResourcesListResponseData = def
      , _resultMcpResourcesListResponseData = def
      }





-- |
--
data McpResourcesReadResponseResult =
  McpResourcesReadResponseResult {
    _contentsMcpResourcesReadResponseResult :: [RawJsonByteString]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesReadResponseResult", omitNothingFields = True} ''McpResourcesReadResponseResult)
makeLenses ''McpResourcesReadResponseResult

instance Default McpResourcesReadResponseResult where
  def = McpResourcesReadResponseResult {
        _contentsMcpResourcesReadResponseResult = def
      }

-- |
--
data McpResourcesReadResponseData =
  McpResourcesReadResponseData {
    _jsonrpcMcpResourcesReadResponseData :: JsonRpcRequest
  , _resultMcpResourcesReadResponseData  :: McpResourcesReadResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesReadResponseData", omitNothingFields = True} ''McpResourcesReadResponseData)
makeLenses ''McpResourcesReadResponseData

instance Default McpResourcesReadResponseData where
  def = McpResourcesReadResponseData {
        _jsonrpcMcpResourcesReadResponseData = def
      , _resultMcpResourcesReadResponseData = def
      }




-- |
--
data McpCompleteResponseResult =
  McpCompleteResponseResult {
    _valuesMcpCompleteResponseResult :: [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCompleteResponseResult", omitNothingFields = True} ''McpCompleteResponseResult)
makeLenses ''McpCompleteResponseResult

instance Default McpCompleteResponseResult where
  def = McpCompleteResponseResult {
        _valuesMcpCompleteResponseResult = def
      }

-- |
--
data McpCompleteResponseData =
  McpCompleteResponseData {
    _jsonrpcMcpCompleteResponseData :: JsonRpcRequest
  , _resultMcpCompleteResponseData  :: McpCompleteResponseResult
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpCompleteResponseData", omitNothingFields = True} ''McpCompleteResponseData)
makeLenses ''McpCompleteResponseData

instance Default McpCompleteResponseData where
  def = McpCompleteResponseData {
        _jsonrpcMcpCompleteResponseData = def
      , _resultMcpCompleteResponseData = def
      }


-- |
--
data McpTextContent =
  McpTextContent {
    _typeMcpTextContent :: String
  , _textMcpTextContent :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpTextContent", omitNothingFields = True} ''McpTextContent)
makeLenses ''McpTextContent

instance Default McpTextContent where
  def = McpTextContent {
        _typeMcpTextContent = "text"
      , _textMcpTextContent = def
      }

-- |
--
data TextResourceContents =
  TextResourceContents {
    _uriTextResourceContents :: String
  , _mimeTypeTextResourceContents :: Maybe String
  , _textTextResourceContents :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "TextResourceContents", omitNothingFields = True} ''TextResourceContents)
makeLenses ''TextResourceContents

instance Default TextResourceContents where
  def = TextResourceContents {
        _uriTextResourceContents = def
      , _mimeTypeTextResourceContents = def
      , _textTextResourceContents = def
      }

-- |
--
data BlobResourceContents =
  BlobResourceContents {
    _uriBlobResourceContents :: String
  , _mimeTypeBlobResourceContents :: Maybe String
  , _blobResourceContents :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "BlobResourceContents", omitNothingFields = True} ''BlobResourceContents)
makeLenses ''BlobResourceContents

instance Default BlobResourceContents where
  def = BlobResourceContents {
        _uriBlobResourceContents = def
      , _mimeTypeBlobResourceContents = def
      , _blobResourceContents = def
      }

-- |
--
data McpResponse =
    McpInitializeResponse McpInitializeResponseData
  | McpToolsListResponse McpToolsListResponseData
  | McpToolsCallResponse McpToolsCallResponseData
  | McpPromptsListResponse McpPromptsListResponseData
  | McpPromptsGetResponse McpPromptsGetResponseData
  | McpResourcesTemplatesListResponse McpResourcesTemplatesListResponseData
  | McpResourcesListResponse McpResourcesListResponseData
  | McpResourcesReadResponse McpResourcesReadResponseData
  | McpCompleteResponse McpCompleteResponseData
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- |
--
data McpToolsListChangedNotificationDataParams =
  McpToolsListChangedNotificationDataParams {
    _toolsMcpToolsListChangedNotificationDataParams :: RawJsonByteString
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListChangedNotificationDataParams", omitNothingFields = True} ''McpToolsListChangedNotificationDataParams)
makeLenses ''McpToolsListChangedNotificationDataParams

instance Default McpToolsListChangedNotificationDataParams where
  def = McpToolsListChangedNotificationDataParams {
        _toolsMcpToolsListChangedNotificationDataParams = RawJsonByteString "[]"
      }

-- |
--
data McpToolsListChangedNotificationData =
  McpToolsListChangedNotificationData {
    _methodMcpToolsListChangedNotificationData  :: String
  , _paramsMcpToolsListChangedNotificationData  :: McpToolsListChangedNotificationDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpToolsListChangedNotificationData", omitNothingFields = True} ''McpToolsListChangedNotificationData)
makeLenses ''McpToolsListChangedNotificationData

instance Default McpToolsListChangedNotificationData where
  def = McpToolsListChangedNotificationData {
        _methodMcpToolsListChangedNotificationData = "notifications/tools/list_changed"
      , _paramsMcpToolsListChangedNotificationData = def
      }
      

-- |
--
data McpPromptsListChangedNotificationDataParams =
  McpPromptsListChangedNotificationDataParams {
    _promptsMcpPromptsListChangedNotificationDataParams :: RawJsonByteString   -- prompts-list
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsListChangedNotificationDataParams", omitNothingFields = True} ''McpPromptsListChangedNotificationDataParams)
makeLenses ''McpPromptsListChangedNotificationDataParams

instance Default McpPromptsListChangedNotificationDataParams where
  def = McpPromptsListChangedNotificationDataParams {
        _promptsMcpPromptsListChangedNotificationDataParams = RawJsonByteString "[]"  -- prompts-list
      }

-- |
--
data McpPromptsListChangedNotificationData =
  McpPromptsListChangedNotificationData {
    _methodMcpPromptsListChangedNotificationData  :: String
  , _paramsMcpPromptsListChangedNotificationData  :: McpPromptsListChangedNotificationDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpPromptsListChangedNotificationData", omitNothingFields = True} ''McpPromptsListChangedNotificationData)
makeLenses ''McpPromptsListChangedNotificationData

instance Default McpPromptsListChangedNotificationData where
  def = McpPromptsListChangedNotificationData {
        _methodMcpPromptsListChangedNotificationData = "notifications/prompts/list_changed"
      , _paramsMcpPromptsListChangedNotificationData = def
      }




-- |
--
data McpResourcesListChangedNotificationDataParams =
  McpResourcesListChangedNotificationDataParams {
    _resourcesMcpResourcesListChangedNotificationDataParams :: RawJsonByteString  -- resources-list
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesListChangedNotificationDataParams", omitNothingFields = True} ''McpResourcesListChangedNotificationDataParams)
makeLenses ''McpResourcesListChangedNotificationDataParams

instance Default McpResourcesListChangedNotificationDataParams where
  def = McpResourcesListChangedNotificationDataParams {
        _resourcesMcpResourcesListChangedNotificationDataParams = RawJsonByteString "[]"   -- resources-list
      }

-- |
--
data McpResourcesListChangedNotificationData =
  McpResourcesListChangedNotificationData {
    _methodMcpResourcesListChangedNotificationData  :: String
  , _paramsMcpResourcesListChangedNotificationData  :: McpResourcesListChangedNotificationDataParams
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = dropDataName "McpResourcesListChangedNotificationData", omitNothingFields = True} ''McpResourcesListChangedNotificationData)
makeLenses ''McpResourcesListChangedNotificationData

instance Default McpResourcesListChangedNotificationData where
  def = McpResourcesListChangedNotificationData {
        _methodMcpResourcesListChangedNotificationData = "notifications/resources/list_changed"
      , _paramsMcpResourcesListChangedNotificationData = def
      }

-- |
--
data McpNotification =
    McpToolsListChangedNotification McpToolsListChangedNotificationData
  | McpPromptsListChangedNotification McpPromptsListChangedNotificationData
  | McpResourcesListChangedNotification McpResourcesListChangedNotificationData
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- |
--
type ToolsCallCommandCallback a = ExitCode
                               -> String -- ^ stdout
                               -> String -- ^ stderr
                               -> IO a

-- |
--
data PtyConnectCommandData =
  PtyConnectCommandData {
    _namePtyConnectCommandData :: String
  , _argumentsPtyConnectCommandData :: RawJsonByteString
  , _callbackPtyConnectCommandData  :: ToolsCallCommandCallback ()
  }

makeLenses ''PtyConnectCommandData


-- |
--
data PtyTerminateCommandData =
  PtyTerminateCommandData {
    _callbackPtyTerminateCommandData  :: ToolsCallCommandCallback ()
  }

makeLenses ''PtyTerminateCommandData


-- |
--
data PtyMessageCommandData =
  PtyMessageCommandData {
    _argumentsPtyMessageCommandData :: RawJsonByteString
  , _callbackPtyMessageCommandData  :: ToolsCallCommandCallback ()
  }

makeLenses ''PtyMessageCommandData


-- |
--
type EchoCommandCallback a = String -> IO a

-- |
--
data EchoCommandData =
  EchoCommandData {
    _valueEchoCommandData :: String
  , _callbackEchoCommandData  :: EchoCommandCallback ()
  }

makeLenses ''EchoCommandData

-- |
--
data Command =
    EchoCommand       EchoCommandData
  | PtyConnectCommand PtyConnectCommandData
  | PtyTerminateCommand PtyTerminateCommandData
  | PtyMessageCommand PtyMessageCommandData


--------------------------------------------------------------------------------
-- |
--
data DefaultCmdRunCommandData =
  DefaultCmdRunCommandData {
    _jsonrpcDefaultCmdRunCommandData   :: JsonRpcRequest
  , _nameDefaultCmdRunCommandData      :: String
  , _argumentsDefaultCmdRunCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''DefaultCmdRunCommandData

-- |
--
data EchoCmdRunCommandData =
  EchoCmdRunCommandData {
    _jsonrpcEchoCmdRunCommandData :: JsonRpcRequest
  , _valueEchoCmdRunCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''EchoCmdRunCommandData

-- |
--
data CmdRunCommand =
    EchoCmdRunCommand EchoCmdRunCommandData
  | DefaultCmdRunCommand DefaultCmdRunCommandData
  deriving (Show, Read, Eq)

-- |
--
getJsonRpcCmdRunCommand :: CmdRunCommand -> JsonRpcRequest
getJsonRpcCmdRunCommand (EchoCmdRunCommand    d) = d^.jsonrpcEchoCmdRunCommandData
getJsonRpcCmdRunCommand (DefaultCmdRunCommand d) = d^.jsonrpcDefaultCmdRunCommandData


--------------------------------------------------------------------------------
-- |
--
data EchoFileSystemCommandData =
  EchoFileSystemCommandData {
    _jsonrpcEchoFileSystemCommandData :: JsonRpcRequest
  , _valueEchoFileSystemCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''EchoFileSystemCommandData

-- |
--
data WriteFileFileSystemCommandData =
  WriteFileFileSystemCommandData {
    _jsonrpcWriteFileFileSystemCommandData   :: JsonRpcRequest
  , _argumentsWriteFileFileSystemCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''WriteFileFileSystemCommandData

-- |
--
data ReadFileFileSystemCommandData =
  ReadFileFileSystemCommandData {
    _jsonrpcReadFileFileSystemCommandData   :: JsonRpcRequest
  , _argumentsReadFileFileSystemCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''ReadFileFileSystemCommandData

-- |
--
data DirListFileSystemCommandData =
  DirListFileSystemCommandData {
    _jsonrpcDirListFileSystemCommandData   :: JsonRpcRequest
  , _argumentsDirListFileSystemCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''DirListFileSystemCommandData

-- |
--
data FileSystemCommand =
    EchoFileSystemCommand EchoFileSystemCommandData
  | DirListFileSystemCommand DirListFileSystemCommandData
  | ReadFileFileSystemCommand ReadFileFileSystemCommandData
  | WriteFileFileSystemCommand WriteFileFileSystemCommandData
  deriving (Show, Read, Eq)

-- |
--
getJsonRpcFileSystemCommand :: FileSystemCommand -> JsonRpcRequest
getJsonRpcFileSystemCommand (EchoFileSystemCommand d)      = d^.jsonrpcEchoFileSystemCommandData
getJsonRpcFileSystemCommand (DirListFileSystemCommand d)   = d^.jsonrpcDirListFileSystemCommandData
getJsonRpcFileSystemCommand (ReadFileFileSystemCommand d)  = d^.jsonrpcReadFileFileSystemCommandData
getJsonRpcFileSystemCommand (WriteFileFileSystemCommand d) = d^.jsonrpcWriteFileFileSystemCommandData

--------------------------------------------------------------------------------
-- |
--
data EchoWatchCommandData =
  EchoWatchCommandData {
    _jsonrpcEchoWatchCommandData :: JsonRpcRequest
  , _valueEchoWatchCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''EchoWatchCommandData

-- |
--
data ToolsListWatchCommandData =
  ToolsListWatchCommandData {
  } deriving (Show, Read, Eq)

makeLenses ''ToolsListWatchCommandData

-- |
--
data PromptsListWatchCommandData =
  PromptsListWatchCommandData {
  } deriving (Show, Read, Eq)

makeLenses ''PromptsListWatchCommandData

-- |
--
data ResourcesListWatchCommandData =
  ResourcesListWatchCommandData {
  } deriving (Show, Read, Eq)

makeLenses ''ResourcesListWatchCommandData


-- |
--
data WatchCommand =
    EchoWatchCommand EchoWatchCommandData
  | ToolsListWatchCommand ToolsListWatchCommandData
  | PromptsListWatchCommand PromptsListWatchCommandData
  | ResourcesListWatchCommand ResourcesListWatchCommandData
  deriving (Show, Read, Eq)



--------------------------------------------------------------------------------
-- |
--
data ProcEchoCommandData =
  ProcEchoCommandData {
    _jsonrpcProcEchoCommandData :: JsonRpcRequest
  , _valueProcEchoCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''ProcEchoCommandData

-- |
--
data ProcRunCommandData =
  ProcRunCommandData {
    _jsonrpcProcRunCommandData   :: JsonRpcRequest
  , _nameProcRunCommandData      :: String
  , _argumentsProcRunCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''ProcRunCommandData


-- |
--
data ProcTerminateCommandData =
  ProcTerminateCommandData {
    _jsonrpcProcTerminateCommandData :: JsonRpcRequest
  } deriving (Show, Read, Eq)
  
makeLenses ''ProcTerminateCommandData


-- |
--
data ProcMessageCommandData =
  ProcMessageCommandData {
    _jsonrpcProcMessageCommandData   :: JsonRpcRequest
  , _argumentsProcMessageCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''ProcMessageCommandData

-- |
--
data ProcSpawnCommand =
    ProcEchoCommand      ProcEchoCommandData
  | ProcRunCommand       ProcRunCommandData
  | ProcTerminateCommand ProcTerminateCommandData
  | ProcMessageCommand   ProcMessageCommandData
  deriving (Show, Read, Eq)

-- |
--
getJsonRpcProcSpawnCommand :: ProcSpawnCommand -> JsonRpcRequest
getJsonRpcProcSpawnCommand (ProcEchoCommand      d) = d^.jsonrpcProcEchoCommandData
getJsonRpcProcSpawnCommand (ProcRunCommand       d) = d^.jsonrpcProcRunCommandData
getJsonRpcProcSpawnCommand (ProcTerminateCommand d) = d^.jsonrpcProcTerminateCommandData
getJsonRpcProcSpawnCommand (ProcMessageCommand   d) = d^.jsonrpcProcMessageCommandData


--------------------------------------------------------------------------------
-- |
--
data SocketEchoCommandData =
  SocketEchoCommandData {
    _jsonrpcSocketEchoCommandData :: JsonRpcRequest
  , _valueSocketEchoCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''SocketEchoCommandData

-- |
--
data SocketOpenCommandData =
  SocketOpenCommandData {
    _jsonrpcSocketOpenCommandData   :: JsonRpcRequest
  , _nameSocketOpenCommandData      :: String
  , _argumentsSocketOpenCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SocketOpenCommandData


-- |
--
data SocketTelnetCommandData =
  SocketTelnetCommandData {
    _jsonrpcSocketTelnetCommandData   :: JsonRpcRequest
  , _nameSocketTelnetCommandData      :: String
  , _argumentsSocketTelnetCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SocketTelnetCommandData

-- |
--
data SocketCloseCommandData =
  SocketCloseCommandData {
    _jsonrpcSocketCloseCommandData :: JsonRpcRequest
  } deriving (Show, Read, Eq)
  
makeLenses ''SocketCloseCommandData


-- |
--
data SocketReadCommandData =
  SocketReadCommandData {
    _jsonrpcSocketReadCommandData :: JsonRpcRequest
  , _argumentsSocketReadCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)
  
makeLenses ''SocketReadCommandData

-- |
--
data SocketWriteCommandData =
  SocketWriteCommandData {
    _jsonrpcSocketWriteCommandData :: JsonRpcRequest
  , _argumentsSocketWriteCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SocketWriteCommandData


-- |
--
data SocketMessageCommandData =
  SocketMessageCommandData {
    _jsonrpcSocketMessageCommandData   :: JsonRpcRequest
  , _argumentsSocketMessageCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SocketMessageCommandData

-- |
--
data SocketCommand =
    SocketEchoCommand    SocketEchoCommandData
  | SocketOpenCommand    SocketOpenCommandData
  | SocketCloseCommand   SocketCloseCommandData
  | SocketReadCommand    SocketReadCommandData
  | SocketWriteCommand   SocketWriteCommandData
  | SocketMessageCommand SocketMessageCommandData
  | SocketTelnetCommand  SocketTelnetCommandData
  deriving (Show, Read, Eq)

-- |
--
getJsonRpcSocketCommand :: SocketCommand -> JsonRpcRequest
getJsonRpcSocketCommand (SocketEchoCommand    d) = d^.jsonrpcSocketEchoCommandData
getJsonRpcSocketCommand (SocketOpenCommand    d) = d^.jsonrpcSocketOpenCommandData
getJsonRpcSocketCommand (SocketCloseCommand   d) = d^.jsonrpcSocketCloseCommandData
getJsonRpcSocketCommand (SocketReadCommand    d) = d^.jsonrpcSocketReadCommandData
getJsonRpcSocketCommand (SocketWriteCommand   d) = d^.jsonrpcSocketWriteCommandData
getJsonRpcSocketCommand (SocketMessageCommand d) = d^.jsonrpcSocketMessageCommandData
getJsonRpcSocketCommand (SocketTelnetCommand  d) = d^.jsonrpcSocketTelnetCommandData

--------------------------------------------------------------------------------
-- |
--
data SerialEchoCommandData =
  SerialEchoCommandData {
    _jsonrpcSerialEchoCommandData :: JsonRpcRequest
  , _valueSerialEchoCommandData   :: String
  } deriving (Show, Read, Eq)

makeLenses ''SerialEchoCommandData

-- |
--
data SerialOpenCommandData =
  SerialOpenCommandData {
    _jsonrpcSerialOpenCommandData   :: JsonRpcRequest
  , _nameSerialOpenCommandData      :: String
  , _argumentsSerialOpenCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SerialOpenCommandData

-- |
--
data SerialCloseCommandData =
  SerialCloseCommandData {
    _jsonrpcSerialCloseCommandData :: JsonRpcRequest
  } deriving (Show, Read, Eq)
  
makeLenses ''SerialCloseCommandData


-- |
--
data SerialReadCommandData =
  SerialReadCommandData {
    _jsonrpcSerialReadCommandData :: JsonRpcRequest
  , _argumentsSerialReadCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)
  
makeLenses ''SerialReadCommandData

-- |
--
data SerialWriteCommandData =
  SerialWriteCommandData {
    _jsonrpcSerialWriteCommandData :: JsonRpcRequest
  , _argumentsSerialWriteCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SerialWriteCommandData


-- |
--
data SerialMessageCommandData =
  SerialMessageCommandData {
    _jsonrpcSerialMessageCommandData   :: JsonRpcRequest
  , _argumentsSerialMessageCommandData :: RawJsonByteString
  } deriving (Show, Read, Eq)

makeLenses ''SerialMessageCommandData

-- |
--
data SerialCommand =
    SerialEchoCommand    SerialEchoCommandData
  | SerialOpenCommand    SerialOpenCommandData
  | SerialCloseCommand   SerialCloseCommandData
  | SerialReadCommand    SerialReadCommandData
  | SerialWriteCommand   SerialWriteCommandData
  | SerialMessageCommand SerialMessageCommandData
  deriving (Show, Read, Eq)


-- |
--
getJsonRpcSerialCommand :: SerialCommand -> JsonRpcRequest
getJsonRpcSerialCommand (SerialEchoCommand    d) = d^.jsonrpcSerialEchoCommandData
getJsonRpcSerialCommand (SerialOpenCommand    d) = d^.jsonrpcSerialOpenCommandData
getJsonRpcSerialCommand (SerialCloseCommand   d) = d^.jsonrpcSerialCloseCommandData
getJsonRpcSerialCommand (SerialReadCommand    d) = d^.jsonrpcSerialReadCommandData
getJsonRpcSerialCommand (SerialWriteCommand   d) = d^.jsonrpcSerialWriteCommandData
getJsonRpcSerialCommand (SerialMessageCommand d) = d^.jsonrpcSerialMessageCommandData

--------------------------------------------------------------------------------
-- |
--
data DomainData = DomainData {
    _logDirDomainData            :: Maybe String
  , _logLevelDomainData          :: LogLevel
  , _toolsDirDomainData          :: String
  , _promptsDirDomainData        :: String
  , _resourcesDirDomainData      :: String
  , _writableDirDomainData       :: Maybe String
  , _requestQueueDomainData      :: TQueue McpRequest
  , _responseQueueDomainData     :: TQueue McpResponse
  , _notificationQueueDomainData :: TQueue McpNotification
  , _commandQueueDomainData      :: TQueue Command
  , _cmdRunQueueDomainData       :: TQueue CmdRunCommand
  , _fileSystemQueueDomainData   :: TQueue FileSystemCommand
  , _watchQueueDomainData        :: TQueue WatchCommand
  , _procspawnQueueDomainData    :: TQueue ProcSpawnCommand
  , _socketQueueDomainData       :: TQueue SocketCommand
  , _serialQueueDomainData       :: TQueue SerialCommand
  , _promptsDomainData           :: [String]
  , _invalidCharsDomainData      :: [String]
  , _invalidCmdsDomainData       :: [String]
  }

makeLenses ''DomainData

defaultDomainData :: IO DomainData
defaultDomainData = do
  hSetEncoding stdin  utf8
  hSetBuffering stdin LineBuffering

  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering

  hSetEncoding stderr utf8
  hSetBuffering stderr LineBuffering

  reqQ <- newTQueueIO
  resQ <- newTQueueIO
  notQ <- newTQueueIO
  cmdQ <- newTQueueIO
  cmdRunQ <- newTQueueIO
  fileSystemQ <- newTQueueIO
  watchQ  <- newTQueueIO
  procQ   <- newTQueueIO
  socketQ   <- newTQueueIO
  serialQ   <- newTQueueIO
  return DomainData {
           _logDirDomainData            = Nothing
         , _logLevelDomainData          = LevelDebug
         , _toolsDirDomainData          = "pty-mcp-server/tools"
         , _promptsDirDomainData        = "pty-mcp-server/prompts"
         , _resourcesDirDomainData      = "pty-mcp-server/resources"
         , _writableDirDomainData       = Nothing
         , _requestQueueDomainData      = reqQ
         , _responseQueueDomainData     = resQ
         , _notificationQueueDomainData = notQ
         , _commandQueueDomainData      = cmdQ
         , _cmdRunQueueDomainData       = cmdRunQ
         , _fileSystemQueueDomainData   = fileSystemQ
         , _watchQueueDomainData        = watchQ
         , _procspawnQueueDomainData    = procQ
         , _socketQueueDomainData       = socketQ
         , _serialQueueDomainData       = serialQ
         , _promptsDomainData           = def
         , _invalidCharsDomainData      = def
         , _invalidCmdsDomainData       = def
         }

-- |
--
type ErrorData = String
type DomainContext a = DomainData -> IO a



