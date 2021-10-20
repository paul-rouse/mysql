{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception                (bracket)
import           Data.Time.Calendar               (fromGregorian)
import           Data.Time.LocalTime              (LocalTime (..),
                                                   TimeOfDay (..))
import           Database.MySQL.Base              (ConnectInfo (..),
                                                   Option (..), bindParams,
                                                   close, connect,
                                                   defaultConnectInfo, execute,
                                                   fetchRow, freeResult,
                                                   initLibrary, initThread,
                                                   prepare, query, storeResult)
import           Database.MySQL.PreparedStatement (Value (..), fetchResults)
import           Test.Hspec

-- This is how to connect to our test database
-- Options with bytestring values are given to partially test #17 and #23
testConn :: ConnectInfo
testConn = defaultConnectInfo {
               connectHost     = "127.0.0.1",
               connectPort = 33306,
               connectUser     = "travis",
               connectPassword     = "esqutest",
               connectDatabase = "esqutest",
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
        result <- storeResult conn
        row <- fetchRow result
        row `shouldBe` [Just "2"]

      it "supports prepared statements" $ do
        stmt <- prepare conn "select 1 where ? = 1"
        bindParams stmt [Long 1]
        execute stmt
        rows <- fetchResults stmt
        rows `shouldBe` [[Just $ LongLong 1]]

      it "supports prepared statements executing multiple times" $ do
        stmt <- prepare conn "select 1 where ? = 1"

        bindParams stmt [Long 2]
        execute stmt
        rows <- fetchResults stmt
        rows `shouldBe` []

        bindParams stmt [Long 1]
        execute stmt
        rows' <- fetchResults stmt
        rows' `shouldBe` [[Just $ LongLong 1]]


