{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, RecordWildCards #-}

module Database.MySQL
    (
    -- * Types
      ConnectInfo(..)
    , Option(..)
    , defaultConnectInfo
    , Connection
    , Result(resConnection, resFields)
    , Field
    , Type
    , MySQLError(errFunction, errNumber, errMessage)
    -- * Connection management
    , connect
    , close
    , autocommit
    , ping
    , changeUser
    , selectDB
    , setCharacterSet
    -- ** Connection information
    , threadId
    , serverInfo
    , hostInfo
    , protocolInfo
    , characterSet
    , sslCipher
    , serverStatus
    -- * Querying
    , query
    -- ** Escaping
    , escape
    -- ** Results
    , fieldCount
    , affectedRows
    -- * Working with results
    , storeResult
    , fetchRow
    , fetchFields
    -- * General information
    , clientInfo
    , clientVersion
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal
import Data.ByteString.Unsafe
    
import Control.Applicative
import Data.Int
import Data.Typeable (Typeable)
import Control.Exception
import Control.Monad
import Database.MySQL.C
import System.IO.Unsafe
import Data.IORef
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Array
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

data Result = Result {
      resFP :: ForeignPtr MYSQL_RES
    , resFields :: {-# UNPACK #-} !Int
    , resConnection :: Connection
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
    connectionError_ "connect" ptr0
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
            withRTSSignalsBlocked (mysql_ping ptr) >>= check "ping" conn

threadId :: Connection -> IO Word
threadId conn = fromIntegral <$> withConn conn mysql_thread_id

serverInfo :: Connection -> IO String
serverInfo conn = withConn conn $ \ptr ->
                  peekCString =<< mysql_get_server_info ptr

hostInfo :: Connection -> IO String
hostInfo conn = withConn conn $ \ptr ->
                peekCString =<< mysql_get_host_info ptr

protocolInfo :: Connection -> IO Word
protocolInfo conn = withConn conn $ \ptr ->
                    fromIntegral <$> mysql_get_proto_info ptr

setCharacterSet :: Connection -> String -> IO ()
setCharacterSet conn cs =
  withCString cs $ \ccs ->
    withConn conn $ \ptr ->
        mysql_set_character_set ptr ccs >>= check "setCharacterSet" conn

characterSet :: Connection -> IO String
characterSet conn = withConn conn $ \ptr ->
  peekCString =<< mysql_character_set_name ptr

sslCipher :: Connection -> IO (Maybe String)
sslCipher conn = withConn conn $ \ptr ->
  withPtr peekCString =<< mysql_get_ssl_cipher ptr

serverStatus :: Connection -> IO String
serverStatus conn = withConn conn $ \ptr -> do
  st <- withRTSSignalsBlocked $ mysql_stat ptr
  checkNull "serverStatus" conn st
  peekCString st

clientInfo :: String
clientInfo = unsafePerformIO $ peekCString mysql_get_client_info
{-# NOINLINE clientInfo #-}

clientVersion :: Word
clientVersion = fromIntegral mysql_get_client_version
{-# NOINLINE clientVersion #-}

autocommit :: Connection -> Bool -> IO ()
autocommit conn onOff = withConn conn $ \ptr ->
   withRTSSignalsBlocked (mysql_autocommit ptr b) >>= check "autocommit" conn
 where b = if onOff then 1 else 0

changeUser :: Connection -> String -> String -> Maybe String -> IO ()
changeUser conn user pass mdb =
  withCString user $ \cuser ->
   withCString pass $ \cpass ->
    withMaybeString mdb $ \cdb ->
     withConn conn $ \ptr ->
      withRTSSignalsBlocked (mysql_change_user ptr cuser cpass cdb) >>=
      check "changeUser" conn

selectDB :: Connection -> String -> IO ()
selectDB conn db = 
  withCString db $ \cdb ->
    withConn conn $ \ptr ->
      withRTSSignalsBlocked (mysql_select_db ptr cdb) >>= check "selectDB" conn

query :: Connection -> ByteString -> IO ()
query conn q = withConn conn $ \ptr ->
  unsafeUseAsCStringLen q $ \(p,l) ->
  mysql_real_query ptr p (fromIntegral l) >>= check "query" conn

fieldCount :: Connection -> IO Int
fieldCount conn = withConn conn $ fmap fromIntegral . mysql_field_count

affectedRows :: Connection -> IO Int64
affectedRows conn = withConn conn $ fmap fromIntegral . mysql_affected_rows

storeResult :: Connection -> IO (Maybe Result)
storeResult conn = withConn conn $ \ptr -> do
  res <- withRTSSignalsBlocked $ mysql_store_result ptr
  fields <- mysql_field_count ptr
  if res == nullPtr
    then if fields == 0
         then return Nothing
         else connectionError "storeResult" conn
    else do
      fp <- newForeignPtr res $ mysql_free_result res
      return . Just $ Result {
                   resFP = fp
                 , resFields = fromIntegral fields
                 , resConnection = conn
                 }

fetchRow :: Result -> IO [Maybe ByteString]
fetchRow res@Result{..}
    | resFields == 0 = return []
    | otherwise      = withRes res $ \ptr -> do
  rowPtr <- mysql_fetch_row ptr
  if rowPtr == nullPtr
    then return []
    else do
      lenPtr <- mysql_fetch_lengths ptr
      checkNull "fetchRow" resConnection lenPtr
      let go len = withPtr $ \colPtr ->
                   create (fromIntegral len) $ \d ->
                   memcpy d (castPtr colPtr) (fromIntegral len)
      sequence =<< zipWith go <$> peekArray resFields lenPtr
                              <*> peekArray resFields rowPtr

fetchFields :: Result -> IO [Field]
fetchFields res = withRes res $ \ptr -> do
  fptr <- withRTSSignalsBlocked $ mysql_fetch_fields ptr
  n <- fieldCount (resConnection res)
  peekArray n fptr

escape :: Connection -> ByteString -> IO ByteString
escape conn bs = withConn conn $ \ptr ->
  unsafeUseAsCStringLen bs $ \(p,l) ->
    createAndTrim (l*2 + 1) $ \to ->
      fromIntegral <$> mysql_real_escape_string ptr (castPtr to) p
                                                (fromIntegral l)

withConn :: Connection -> (Ptr MYSQL -> IO a) -> IO a
withConn conn = withForeignPtr (connFP conn)

withRes :: Result -> (Ptr MYSQL_RES -> IO a) -> IO a
withRes res = withForeignPtr (resFP res)

withString :: String -> (CString -> IO a) -> IO a
withString [] act = act nullPtr
withString xs act = withCString xs act

withMaybeString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeString Nothing act = act nullPtr
withMaybeString (Just xs) act = withCString xs act

check :: Num a => String -> Connection -> a -> IO ()
check func conn r = unless (r == 0) $ connectionError func conn
{-# INLINE check #-}

checkNull :: String -> Connection -> Ptr a -> IO ()
checkNull func conn p = when (p == nullPtr) $ connectionError func conn
{-# INLINE checkNull #-}

withPtr :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
withPtr act p | p == nullPtr = return Nothing
              | otherwise    = Just <$> act p

connectionError :: String -> Connection -> IO a
connectionError func conn = withConn conn $ connectionError_ func

connectionError_ :: String -> Ptr MYSQL -> IO a
connectionError_ func ptr =do
  errno <- mysql_errno ptr
  msg <- peekCString =<< mysql_error ptr
  throw $ ConnectionError func (fromIntegral errno) msg
