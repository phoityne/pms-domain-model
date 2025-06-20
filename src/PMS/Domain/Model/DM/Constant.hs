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
_TOOLS_LIST_FILE :: String
_TOOLS_LIST_FILE = "tools-list.json"

