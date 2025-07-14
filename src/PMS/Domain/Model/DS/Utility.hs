{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module PMS.Domain.Model.DS.Utility where

import System.Log.FastLogger
import Control.Monad.Logger
import Control.Lens
import System.FilePath
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Control.Exception.Safe as E
import qualified Data.Text as T
import Data.Char 
import Control.Monad
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.String.AnsiEscapeCodes.Strip.Text as ANSI
import qualified Data.Text.Encoding.Error as TEE
import qualified Control.Concurrent.STM as STM
import System.IO
import Data.List

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


-- |
--  [ ";", "&&", "|", "$", "`", "<", ">", "\\", "\"", "..", "/"]
--
invalidChars :: [T.Text]
invalidChars =
#ifdef mingw32_HOST_OS
  [ "&&", "||", "|", ".."]
#else
  [ "&&", "||", "|", ".."]
#endif

-- |
--
invalidCmds :: [String]
invalidCmds = 
#ifdef mingw32_HOST_OS
  [
    "del", "erase", "rd", "rmdir", "format"
  , "shutdown", "restart", "taskkill"
  ]
#else
  [
    "rm", "mv", "dd", "chmod", "chown"
  , "shutdown", "reboot", "kill", "nc"
  ]
#endif


-- |
--
validateMessage :: String -> IO String
validateMessage cmd = case words cmd of
  [] -> return cmd
  (c : args) -> do
    _ <- validateCommand c
    _ <- validateArgs args
    return cmd

-- |
--
validateCommand :: String -> IO String
validateCommand cmd = do
  let tcmd = T.pack cmd

  mapM_ (\seqStr ->
          when (seqStr `T.isInfixOf` tcmd) $
            E.throwString $ "Command contains forbidden sequence: " ++ T.unpack seqStr
        ) invalidChars

  when (cmd `elem` invalidCmds) $
    E.throwString $ "Command is forbidden: " ++ cmd

  when (any (not . isAllowedChar) cmd) $
    E.throwString $ "Command contains disallowed characters: " ++ cmd

  return cmd
  where
    isAllowedChar :: Char -> Bool
    isAllowedChar c = isAlphaNum c || c `elem` ("-._" :: String)


-- |
--
validateArgs :: [String] -> IO [String]
validateArgs = mapM validateArg

-- |
--
validateArg :: String -> IO String
validateArg arg = do
  let tArg = T.pack arg
  when (any (`T.isInfixOf` tArg) invalidChars) $
    E.throwString $ "Argument contains forbidden sequences: " ++ arg
  return arg



-- |
--
expect :: STM.TMVar () -> IO BS.ByteString -> [String] -> IO (Maybe String)
expect lock feed prompts = STM.atomically (STM.tryTakeTMVar lock) >>= \case
  Nothing -> do
    hPutStrLn stderr "[INFO] expect running. skip."
    return Nothing
  Just () -> flip E.catchAny exception $ flip E.finally finalize $ do
    hPutStrLn stderr $ "[INFO] expect: " ++ show prompts
    output <- readUntilPrompt feed prompts
    
    -- let result = T.unpack (TE.decodeUtf8 output)
    let result = T.unpack $ ANSI.stripAnsiEscapeCodes $ TE.decodeUtf8With TEE.lenientDecode output
    return (Just result)

  where
    -- |
    --
    exception :: E.SomeException -> IO (Maybe String)
    exception e = do
      hPutStrLn stderr $ "[ERROR] expect exception: " ++ show e
      return . Just . show $ e

    -- |
    --
    finalize :: IO ()
    finalize = STM.atomically $ STM.putTMVar lock ()

-- |
--
readUntilPrompt :: IO BS.ByteString -> [String] -> IO BS.ByteString
readUntilPrompt feed prompts = go BS.empty
  where
    (negativePrompts, positivePrompts) = partition (\s -> not (null s) && head s == '!') prompts
    promptBsList    = map BS.pack $ filter (not . null) $ map (dropWhile (== '!')) positivePrompts
    rejectPromptBs  = map BS.pack $ map (drop 1) negativePrompts

    foundPrompt acc =
      any (`BS.isInfixOf` acc) promptBsList &&
      all (not . (`BS.isInfixOf` acc)) rejectPromptBs

    go acc = do
      chunk <- feed
      when ("\ESC[6n" `BS.isInfixOf` chunk) $
        E.throwString "Unsupported: Detected cursor position report request (ESC[6n)."

      let txt = TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      let acc' = BS.append acc chunk
      if foundPrompt acc'
        then return acc'
        else go acc'


{-
readUntilPrompt :: IO BS.ByteString -> [String] -> IO BS.ByteString
readUntilPrompt feed prompts = go BS.empty
  where
    promptBsList = map BS.pack prompts

    foundPrompt acc = any (`BS.isInfixOf` acc) promptBsList

    go acc = do
      chunk <- feed
      when ("\ESC[6n" `BS.isInfixOf` chunk) $ do
        E.throwString "Unsupported: Detected cursor position report request (ESC[6n)."

      -- let txt = ANSI.stripAnsiEscapeCodes $ TE.decodeUtf8With TEE.lenientDecode chunk
      let txt = TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      let acc' = BS.append acc chunk
      if foundPrompt acc'
        then return acc'
        else go acc'



readUntilPrompt :: Pty -> [String] -> IO BS.ByteString
readUntilPrompt pms prompts = go BS.empty T.empty
  where
    promptTextList = map T.pack prompts

    foundPrompt :: T.Text -> Bool
    foundPrompt txt = any (`T.isInfixOf` txt) promptTextList

    go accBs accTxt = do
      chunk <- readPty pms
      let txt = ANSI.stripAnsiEscapeCodes $ TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      when ("\ESC[6n" `BS.isInfixOf` chunk) $ do
        hPutStrLn stderr "[INFO] Detected cursor position report request, replying with ESC[1;1R"
        writePty pms (DBS.pack ([0x1B :: Word8, 0x5B :: Word8] ++ map (fromIntegral . ord) "0;0R"))

      let accBs' = BS.append accBs chunk
          accTxt' = T.append accTxt txt
      if foundPrompt accTxt'
        then return accBs'
        else go accBs' accTxt'
-}
