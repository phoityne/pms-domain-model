module PMS.Domain.Model.DM.Constant where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------
-- |
--
_LOGTAG :: T.Text
_LOGTAG = T.pack ""

-- |
--
_TIME_FORMAT :: B.ByteString
_TIME_FORMAT = T.encodeUtf8 $ T.pack "%Y/%m/%d %T %z"

-- |
--
_TIMEOUT_MICROSEC :: Int
_TIMEOUT_MICROSEC = 30 * 1000 * 1000

-- |
--
_TOOLS_LIST_FILE :: String
_TOOLS_LIST_FILE = "tools-list.json"

-- |
--
_PROMPTS_LIST_FILE :: String
_PROMPTS_LIST_FILE = "prompts-list.json"

-- |
--
_RESOURCES_LIST_FILE :: String
_RESOURCES_LIST_FILE = "resources-list.json"

-- |
--
_RESOURCES_TPL_LIST_FILE :: String
_RESOURCES_TPL_LIST_FILE = "resources-templates-list.json"

-- |
--
_LF :: String
_LF = "\n"

-- |
--
_CRLF :: String
_CRLF = "\r\n"

