{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Domain.Model.DS.UtilitySpec (spec) where

import Test.Hspec
import Control.Lens
import Data.Aeson
import System.Exit (ExitCode(..))
import System.IO
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified PMS.Domain.Model.DS.Utility as SUT
import qualified PMS.Domain.Model.DM.Type as SUT
--
data SpecContext = SpecContext {}

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  return SpecContext {}

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp _ = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."

  ctx <- defaultSpecContext
  return ctx

-- |
--
tearDown :: SpecContext -> IO ()
tearDown _ = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."

-- |
--
run :: SpecWith SpecContext
run = do
  describe "str2lbs" $ do
    context "abc" $ do
      it "should be abc" $ \_ -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let expect = "abc"
            actual = SUT.lbs2str . SUT.str2lbs $ expect

        actual `shouldBe` expect

  -- CR-11: Handle I/O common utility tests

  describe "bytesToHex" $ do
    -- TC-DM-04
    context "bytes [0x00, 0x0A, 0x1B, 0xFF]" $ do
      it "should return '000A1BFF'" $ \_ -> do
        let input  = BS.pack [0x00, 0x0A, 0x1B, 0xFF]
            actual = SUT.bytesToHex input
        actual `shouldBe` "000A1BFF"

  describe "str2bsUTF8" $ do
    -- TC-DM-05
    context "hello" $ do
      it "should encode to UTF-8 ByteString" $ \_ -> do
        let actual = SUT.str2bsUTF8 "hello"
        actual `shouldBe` BS.pack [0x68, 0x65, 0x6C, 0x6C, 0x6F]

  describe "bs2strUTF8" $ do
    -- TC-DM-06: no ANSI stripping
    context "UTF-8 ByteString without ANSI" $ do
      it "should decode to String" $ \_ -> do
        let input  = BS.pack [0x68, 0x65, 0x6C, 0x6C, 0x6F]
            actual = SUT.bs2strUTF8 input
        actual `shouldBe` "hello"
    context "UTF-8 ByteString with ANSI escape" $ do
      it "should decode without stripping ANSI" $ \_ -> do
        -- ESC[31m = red color escape sequence
        let input  = BS.pack [0x1B, 0x5B, 0x33, 0x31, 0x6D, 0x68, 0x69]
            actual = SUT.bs2strUTF8 input
        actual `shouldBe` "\ESC[31mhi"

  describe "bs2strUTF8Strip" $ do
    -- TC-DM-07: ANSI stripped
    context "UTF-8 ByteString with ANSI escape" $ do
      it "should decode and strip ANSI sequences" $ \_ -> do
        let input  = BS.pack [0x1B, 0x5B, 0x33, 0x31, 0x6D, 0x68, 0x69]
            actual = SUT.bs2strUTF8Strip input
        actual `shouldBe` "hi"

  describe "toolsCallResponse" $ do
    -- TC-DM-08
    context "ExitSuccess" $ do
      it "should write McpToolsCallResponse with isError=False to TQueue" $ \_ -> do
        let jsonRpc = SUT.JsonRpcRequest
              { SUT._jsonrpcJsonRpcRequest = "2.0"
              , SUT._idJsonRpcRequest      = Just 1
              , SUT._methodJsonRpcRequest  = "tools/call"
              , SUT._paramsJsonRpcRequest  = Nothing
              }
        resQ <- STM.newTQueueIO
        SUT.toolsCallResponse resQ jsonRpc ExitSuccess "out" "err"
        res <- STM.atomically $ STM.readTQueue resQ
        case res of
          SUT.McpToolsCallResponse dat ->
            SUT._isErrorMcpToolsCallResponseResult
              (SUT._resultMcpToolsCallResponseData dat) `shouldBe` False
          _ -> expectationFailure "unexpected McpResponse variant"
    -- TC-DM-09
    context "ExitFailure" $ do
      it "should write McpToolsCallResponse with isError=True to TQueue" $ \_ -> do
        let jsonRpc = SUT.JsonRpcRequest
              { SUT._jsonrpcJsonRpcRequest = "2.0"
              , SUT._idJsonRpcRequest      = Just 1
              , SUT._methodJsonRpcRequest  = "tools/call"
              , SUT._paramsJsonRpcRequest  = Nothing
              }
        resQ <- STM.newTQueueIO
        SUT.toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "error"
        res <- STM.atomically $ STM.readTQueue resQ
        case res of
          SUT.McpToolsCallResponse dat ->
            SUT._isErrorMcpToolsCallResponseResult
              (SUT._resultMcpToolsCallResponseData dat) `shouldBe` True
          _ -> expectationFailure "unexpected McpResponse variant"



  -- CR-12: Security check function tests

  describe "checkInvalidPatterns" $ do

    -- TC-DM-10: match exists -> Just pattern
    context "TC-DM-10: input matches a literal pattern" $ do
      it "should return Just the matched pattern" $ \_ -> do
        let patterns = ["rm", "shutdown"]
            input    = "rm -rf /"
            actual   = SUT.checkInvalidPatterns patterns input
        actual `shouldBe` Just "rm"

    -- TC-DM-11: no match -> Nothing
    context "TC-DM-11: input does not match any pattern" $ do
      it "should return Nothing" $ \_ -> do
        let patterns = ["rm", "shutdown"]
            input    = "echo hello"
            actual   = SUT.checkInvalidPatterns patterns input
        actual `shouldBe` Nothing

    -- TC-DM-12: empty pattern list -> Nothing (allow all)
    context "TC-DM-12: empty pattern list" $ do
      it "should return Nothing (allow all)" $ \_ -> do
        let patterns = [] :: [String]
            input    = "rm -rf /"
            actual   = SUT.checkInvalidPatterns patterns input
        actual `shouldBe` Nothing

    -- TC-DM-13: regex pattern ".*ssh.*" matches
    context "TC-DM-13: regex pattern '.*ssh.*' matches ssh command" $ do
      it "should return Just \".*ssh.*\"" $ \_ -> do
        let patterns = [".*ssh.*"]
            input    = "ssh user@host"
            actual   = SUT.checkInvalidPatterns patterns input
        actual `shouldBe` Just ".*ssh.*"

  describe "checkSandboxNetworks" $ do

    -- TC-DM-14: empty CIDR list -> Left (deny all)
    context "TC-DM-14: empty CIDR list" $ do
      it "should return Left (deny all hosts)" $ \_ -> do
        result <- SUT.checkSandboxNetworks [] "127.0.0.1"
        case result of
          Left msg -> msg `shouldContain` "sandboxNetworks is empty"
          Right () -> expectationFailure "Expected Left but got Right"

    -- TC-DM-15: host IP within range -> Right ()
    context "TC-DM-15: loopback IP within 127.0.0.0/8" $ do
      it "should return Right ()" $ \_ -> do
        result <- SUT.checkSandboxNetworks ["127.0.0.0/8"] "127.0.0.1"
        result `shouldBe` Right ()

    -- TC-DM-16: host IP outside all ranges -> Left
    context "TC-DM-16: IP outside all CIDR ranges" $ do
      it "should return Left with error message containing 'outside sandboxNetworks'" $ \_ -> do
        result <- SUT.checkSandboxNetworks ["10.0.0.0/8"] "127.0.0.1"
        case result of
          Left msg -> msg `shouldContain` "outside sandboxNetworks"
          Right () -> expectationFailure "Expected Left but got Right"

    -- TC-DM-17: multiple CIDRs, IP matches one -> Right ()
    context "TC-DM-17: IP matches second of multiple CIDR ranges" $ do
      it "should return Right ()" $ \_ -> do
        result <- SUT.checkSandboxNetworks ["10.0.0.0/8", "127.0.0.0/8"] "127.0.0.1"
        result `shouldBe` Right ()

