{-# LANGUAGE OverloadedStrings          #-}

import Control.Applicative   ((<|>))
import Control.Exception     (bracket)
import Database.MySQL.Base   (ConnectInfo (..), defaultConnectInfo, Option (..),
                              connect, close,
                              query, useResult, fetchRow)
import System.Environment    (getEnvironment)
import Test.Hspec

isCI :: IO Bool
isCI = do
    env <- getEnvironment
    return $ case lookup "TRAVIS" env <|> lookup "CI" env of
               Just "true" -> True
               _ -> False

-- This is how to connect to our test database
-- Options with bytestring values are given to partially test #17 and #23
testConn :: Bool -> ConnectInfo
testConn ci = defaultConnectInfo {
                connectHost     = "127.0.0.1"
              , connectUser     = "test"
              , connectPassword = "test"
              , connectDatabase = "test"
              , connectPort     = if ci then 33306 else 3306
              , connectOptions  = [
                    InitCommand "SET SESSION sql_mode = 'STRICT_ALL_TABLES';"
                  , ReadDefaultGroup "client"
                ]
              }

-- Only the most cursory test is done at the moment, simply showing that
-- things hang together sufficiently well that we can talk to the database
-- server.
--
main :: IO ()
main = do
    ci <- isCI
    bracket (connect $ testConn ci) close $ \conn -> hspec $ do
        describe "Database" $ do
          it "seems to be connected" $ do
            query conn "select 1 + 1"
            result <- useResult conn
            row <- fetchRow result
            row `shouldBe` [Just "2"]
