{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module PMS.Domain.Model.DS.UtilitySpec (spec) where

import Test.Hspec
import Control.Lens
import Data.Aeson

import qualified PMS.Domain.Model.DS.Utility as SUT
import qualified PMS.Domain.Model.DM.Type as SUT


-- |
--
data SpecContext = SpecContext {
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  return SpecContext {
         }

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
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be run" $ \_ -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let expect = "abc"
            actual = SUT.lbs2str . SUT.str2lbs $ expect

        actual `shouldBe` expect

      it "should be json" $ \_ -> do 
        putStrLn "[INFO] EXECUTING THE SECOND TEST."

        let jsonStr = """
                      {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.99.2"},"protocolVersion":"2024-11-05"}}
                      """
            Right jsonDat = (eitherDecode jsonStr) :: Either String SUT.JsonRpcRequest

        let actual = encode jsonDat

        actual `shouldBe` jsonStr


