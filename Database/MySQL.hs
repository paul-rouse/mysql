{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, RecordWildCards #-}

module Database.MySQL
    (
    -- * Types
      ConnectInfo(..)
    , Option(..)
    , defaultConnectInfo
    , Connection
    , MySQLError(errFunction, errNumber, errMessage)
    -- * Connection management
    , connect
    , close
    , ping
    , threadId
    ) where

import Control.Applicative
import Data.Typeable (Typeable)
import Control.Exception
import Control.Monad
import Database.MySQL.C
import Data.IORef
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    , connectOptions :: [Option]
    , connectPath :: FilePath
    } deriving (Eq, Read, Show, Typeable)

data MySQLError = ConnectionError {
      errFunction :: String
    , errNumber :: Int
    , errMessage :: String
    } deriving (Eq, Show, Typeable)

instance Exception MySQLError

data Connection = Connection {
      connFP :: ForeignPtr MYSQL
    , connClose :: Ptr MYSQL -> IO ()
    }

data Option = Option
            deriving (Eq, Read, Show, Typeable)

defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo {
                       connectHost = "localhost"
                     , connectPort = 3306
                     , connectUser = "root"
                     , connectPassword = ""
                     , connectDatabase = "test"
                     , connectOptions = []
                     , connectPath = ""
                     }

connect :: ConnectInfo -> IO Connection
connect ConnectInfo{..} = do
  closed <- newIORef False
  ptr0 <- mysql_init nullPtr
  ptr <- withString connectHost $ \chost ->
          withString connectUser $ \cuser ->
           withString connectPassword $ \cpass ->
            withString connectDatabase $ \cdb ->
             withRTSSignalsBlocked . withString connectPath $
              mysql_real_connect ptr0 chost cuser cpass cdb
                                 (fromIntegral connectPort)
  when (ptr == nullPtr) $
    connectionError "connect" ptr0
  fp <- newForeignPtr ptr $ realClose closed ptr
  return Connection {
               connFP = fp
             , connClose = realClose closed
             }

close :: Connection -> IO ()
close conn = withConn conn (connClose conn)

realClose :: IORef Bool -> Ptr MYSQL -> IO ()
realClose closeInfo ptr = do
  wasClosed <- atomicModifyIORef closeInfo $ \prev -> (True, prev)
  unless wasClosed . withRTSSignalsBlocked $ mysql_close ptr

ping :: Connection -> IO ()
ping conn = withConn conn $ \ptr ->
            withRTSSignalsBlocked (mysql_ping ptr) >>= check "ping" ptr

threadId :: Connection -> IO Word
threadId conn = fromIntegral <$> withConn conn mysql_thread_id

withConn :: Connection -> (Ptr MYSQL -> IO a) -> IO a
withConn conn = withForeignPtr (connFP conn)

withString :: String -> (CString -> IO a) -> IO a
withString [] act = act nullPtr
withString xs act = withCString xs act

check :: String -> Ptr MYSQL -> CInt -> IO ()
check func ptr r = unless (r == 0) $ connectionError func ptr

connectionError :: String -> Ptr MYSQL -> IO a
connectionError func ptr = do
  errno <- mysql_errno ptr
  msg <- peekCString =<< mysql_error ptr
  throw $ ConnectionError func (fromIntegral errno) msg
