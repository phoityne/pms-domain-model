{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Base16 as B16
import System.Exit (ExitCode(..))
import Text.Regex.TDFA ((=~))
import Data.IP (AddrRange, IPv4, IP(..), isMatchedTo, fromSockAddr)
import Network.Socket (getAddrInfo, defaultHints, addrAddress, HostName, SockAddr)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

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
-- invalidChars :: [String]
-- invalidChars = [ "&&", "||", "..", "reboot", "shutdown", "restart", "kill"]

-- |
--
-- invalidCmds :: [String]
-- invalidCmds = [
--     "del", "erase", "rd", "rmdir", "format"
--   , "shutdown", "restart", "taskkill"
--   , "rm", "mv", "dd", "chmod", "chown"
--   , "reboot", "kill", "nc", "sudo", "su"
--   ]

-- |
--
validateMessage :: [String] ->  [String] -> String -> IO String
validateMessage invalidChars invalidCmds cmd = do
  let tcmd = T.pack cmd

  mapM_ (\seqStr ->
          when (T.pack seqStr `T.isInfixOf` tcmd) $
            E.throwString $ "Command contains forbidden sequence: " ++ seqStr
        ) invalidChars

  mapM_ (\seqStr ->
          when (T.pack seqStr `T.isInfixOf` tcmd) $
            E.throwString $ "Command contains forbidden sequence: " ++ seqStr
        ) invalidCmds

  return cmd

-- |
--
validateCommand :: [String] -> [String] -> String -> IO String
validateCommand invalidChars invalidCmds cmd = do
  let tcmd = T.pack cmd

  mapM_ (\seqStr ->
          when (T.pack seqStr `T.isInfixOf` tcmd) $
            E.throwString $ "Command contains forbidden sequence: " ++ seqStr
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
validateArgs :: [String] -> [String] -> IO [String]
validateArgs invalidChars = mapM (validateArg invalidChars)

-- |
--
validateArg :: [String] -> String -> IO String
validateArg invalidChars arg = do
  let tArg = T.pack arg
  when (any (\s -> T.pack s `T.isInfixOf` tArg) invalidChars) $
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
{-
    go acc = do
      let tout = 10*1000*1000
      timeout tout feed >>= \case
        Just chunk -> do
          when ("\ESC[6n" `BS.isInfixOf` chunk) $
            E.throwString "Unsupported: Detected cursor position report request (ESC[6n)."

          let txt = TE.decodeUtf8With TEE.lenientDecode chunk
          hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

          let acc' = BS.append acc chunk
          if foundPrompt acc'
            then return acc'
            else go acc'
        Nothing -> return $ BS.concat [BS.pack "timeout occurred. acc:", acc]
-}


    go :: BS.ByteString -> IO BS.ByteString
    go acc = do

      chunk <- E.catchAny feed (hdlExcept acc)
      let txt = TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      let acc' = BS.append acc chunk
      when ("\ESC[6n" `BS.isInfixOf` chunk) $ do
        let accText = TE.decodeUtf8With TEE.lenientDecode acc'
        E.throwString $ "Unsupported: Detected cursor position report request (ESC[6n)." ++ T.unpack accText

      if foundPrompt acc'
        then return acc'
        else go acc'

    hdlExcept :: BS.ByteString -> E.SomeException -> IO BS.ByteString
    hdlExcept acc e = do
      let accText = TE.decodeUtf8With TEE.lenientDecode acc
      E.throwString $ show e ++ "\nAccumulated input so far:\n" ++ T.unpack accText


--------------------------------------------------------------------------------
-- CR-11: Handle I/O common utilities
--------------------------------------------------------------------------------

-- | Read from a Handle with a timeout. tout is in milliseconds.
-- Returns an empty ByteString on timeout (not an error).
-- Consolidates readProc (agent-process) and readSocket (agent-socket) patterns.
readHandle :: Handle -> Int -> Int -> IO BSS.ByteString
readHandle hdl tout size = do
  ready <- hWaitForInput hdl tout
  if ready
    then BSS.hGetSome hdl size
    else return BSS.empty

-- | Write a ByteString to a Handle and flush.
writeHandle :: Handle -> BSS.ByteString -> IO ()
writeHandle hdl bs = do
  BSS.hPut hdl bs
  hFlush hdl

-- | Convert a ByteString to an uppercase hex string (e.g. "000A1BFF").
bytesToHex :: BSS.ByteString -> String
bytesToHex = map toUpper . BS.unpack . B16.encode

-- | Encode a String to a UTF-8 encoded ByteString.
str2bsUTF8 :: String -> BSS.ByteString
str2bsUTF8 = TE.encodeUtf8 . T.pack

-- | Decode a ByteString to a String using UTF-8 lenient decoding (no ANSI strip).
-- For raw socket or binary data.
bs2strUTF8 :: BSS.ByteString -> String
bs2strUTF8 = T.unpack . TE.decodeUtf8With TEE.lenientDecode

-- | Decode a ByteString to a String using UTF-8 lenient decoding and strip ANSI escapes.
-- For process output (pty / pipe output).
bs2strUTF8Strip :: BSS.ByteString -> String
bs2strUTF8Strip = T.unpack . ANSI.stripAnsiEscapeCodes . TE.decodeUtf8With TEE.lenientDecode

-- | Write a McpResponse to the TQueue.
-- Consolidated here because it depends only on domain-model types.
toolsCallResponse :: STM.TQueue McpResponse
                  -> JsonRpcRequest
                  -> ExitCode
                  -> String
                  -> String
                  -> IO ()
toolsCallResponse resQ jsonRpc code outStr errStr = do
  let content = [ McpToolsCallResponseResultContent "text" outStr
                , McpToolsCallResponseResultContent "text" errStr
                ]
      result = McpToolsCallResponseResult {
                  _contentMcpToolsCallResponseResult  = content
                , _isErrorMcpToolsCallResponseResult  = (ExitSuccess /= code)
                }
      resDat = McpToolsCallResponseData jsonRpc result
      res    = McpToolsCallResponse resDat
  STM.atomically $ STM.writeTQueue resQ res


-- | Append a newline character ('\n') to the string.
-- Simple unconditional append; no escape expansion, no duplicate check.
-- Called only when appendNewline is True (or omitted = default True).
-- write-byte variants (binary) must NOT use this function.
appendCRLF :: String -> String
appendCRLF str = str ++ lineEnding
  where
    lineEnding :: String
    lineEnding = case nativeNewline of
      CRLF -> _CRLF
      LF   -> _LF

--------------------------------------------------------------------------------
-- CR-12: Security check functions
--------------------------------------------------------------------------------

-- | Check input against a list of regex patterns (invalidPatterns).
-- Returns the first matching pattern as Just, or Nothing if no match (allow).
-- An empty pattern list means allow all.
checkInvalidPatterns :: [String] -> String -> Maybe String
checkInvalidPatterns patterns input =
  find (\pat -> (input =~ pat) :: Bool) patterns

-- | Check a hostname against a list of CIDR ranges (sandboxNetworks).
-- Returns Right () if the resolved IP matches any range, Left error otherwise.
-- An empty CIDR list means deny all hosts.
-- Unix Domain Socket connections should NOT call this function.
checkSandboxNetworks :: [String] -> HostName -> IO (Either String ())
checkSandboxNetworks [] host =
  return . Left $
    "agent-socket-open: connection refused. host " ++ host
    ++ " is outside sandboxNetworks (sandboxNetworks is empty; all hosts denied)."
checkSandboxNetworks cidrs host = do
  let ranges = mapMaybe readMaybe cidrs :: [AddrRange IPv4]
  addrs <- getAddrInfo (Just defaultHints) (Just host) Nothing
  -- Case B: classify each resolved address; IPv6 is explicitly rejected.
  let classified = map (classifyAddr . addrAddress) addrs
  case sequence classified of
    Left errMsg -> return (Left errMsg)
    Right ipv4s ->
      if any (\ip -> any (ip `isMatchedTo`) ranges) ipv4s
        then return (Right ())
        else return . Left $
               "agent-socket-open: connection refused. host " ++ host
               ++ " is outside sandboxNetworks (allowed: " ++ show cidrs ++ ")."

-- | Classify a SockAddr as IPv4 (Right) or unsupported (Left error message).
-- IPv6 addresses are explicitly rejected with a descriptive error (Case B).
-- Non-IP addresses should never reach this function (Unix Domain Socket is exempt).
classifyAddr :: SockAddr -> Either String IPv4
classifyAddr sa = case fromSockAddr sa of
  Just (IPv4 v4, _) -> Right v4
  Just (IPv6 v6, _) -> Left $
    "agent-socket-open: IPv6 is not supported in sandboxNetworks. "
    ++ "resolved address: " ++ show v6
    ++ " (use an IPv4 host or IPv4 CIDR ranges)."
  _ -> Left
    "agent-socket-open: unsupported address type returned by DNS resolution."
