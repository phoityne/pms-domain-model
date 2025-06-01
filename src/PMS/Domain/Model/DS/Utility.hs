{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Model.DS.Utility where

import System.Log.FastLogger
import Control.Monad.Logger
import Control.Lens
import System.FilePath
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import PMS.Domain.Model.DM.Type
import PMS.Domain.Model.DM.Constant



-- |
--
runFastLoggerT :: DomainData -> TimedFastLogger -> LoggingT IO a -> IO a
runFastLoggerT dat logger app = do
  let logLevel = dat^.logLevelDomainData
      
  runLoggingT (filterLogger (filterByLevel logLevel) app) $ output logger 

  where
    -- |
    --
    output :: TimedFastLogger
           -> Loc
           -> LogSource
           -> LogLevel
           -> LogStr
           -> IO ()
    output l a b c d = do
      let msg = defaultLogStr a b c d
      l (\ts -> toLogStr ts <> " " <> msg)

    -- |
    --
    filterByLevel :: LogLevel -> LogSource -> LogLevel -> Bool
    filterByLevel target _ actual = actual >= target



-- |
--
createLogger :: DomainData -> String -> IO (TimedFastLogger, IO ())
createLogger dat logFile = newTimeCache _TIME_FORMAT >>= withDir (dat^.logDirDomainData)
  where
    withDir  Nothing  tcache = newTimedFastLogger tcache $ LogStderr defaultBufSize
    withDir (Just dr) tcache = newTimedFastLogger tcache $ LogFileNoRotate (dr </> logFile) defaultBufSize


-- |
--
str2lbs :: String -> BSL.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack

-- |
--
lbs2str :: BSL.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8

