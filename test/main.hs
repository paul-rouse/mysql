{-# LANGUAGE OverloadedStrings          #-}

import Control.Exception     (bracket)
import Database.MySQL.Base   (ConnectInfo (..), defaultConnectInfo, Option (..),
                              connect, close,
                              query, useResult, fetchRow)
import Test.Hspec

-- This is how to connect to our test database
-- Options with bytestring values are given to partially test #17 and #23
testConn :: ConnectInfo
testConn = defaultConnectInfo {
               connectHost     = "127.0.0.1",
               connectUser     = "test",
               connectDatabase = "test",
               connectOptions  = [
                   InitCommand "SET SESSION sql_mode = 'STRICT_ALL_TABLES';"
                 , ReadDefaultGroup "client"
               ]
           }

-- Only the most cursory test is done at the moment, simply showing that
-- things hang together sufficiently well that we can talk to the database
-- server.
--
main :: IO ()
main = bracket (connect testConn) close $ \conn -> hspec $ do
    describe "Database" $ do
      it "seems to be connected" $ do
        query conn "select 1 + 1"
        result <- useResult conn
        row <- fetchRow result
        row `shouldBe` [Just "2"]
