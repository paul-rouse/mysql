{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, RecordWildCards #-}

-- |
-- Module:      Database.MySQL.Base
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- A low-level client library for the MySQL database, implemented as
-- bindings to the C @mysqlclient@ API.
--
-- The C library is thread-safe, but uses thread-local state.  Therefore,
-- if these bindings are used in a multi-threaded program, "bound" threads
-- should be used (see "Control.Concurrent").  In addition, explicit calls
-- to 'initLibrary', and possibly 'initThread' and 'endThread' may be needed
-- in a multi-threaded program.

module Database.MySQL.Base
    (
    -- * Licensing
    -- $license
    -- * Resource management
    -- $mgmt
    -- * Types
      ConnectInfo(..)
    , SSLInfo(..)
    , Seconds
    , Protocol(..)
    , Option(..)
    , defaultConnectInfo
    , defaultSSLInfo
    , Connection
    , Result
    , Type(..)
    , Row
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
    , insertID
    -- ** Escaping
    , escape
    -- ** Results
    , fieldCount
    , affectedRows
    -- * Working with results
    , isResultValid
    , freeResult
    , storeResult
    , useResult
    , fetchRow
    , fetchFields
    , dataSeek
    , rowSeek
    , rowTell
    -- ** Multiple results
    , nextResult
    -- * Transactions
    , commit
    , rollback
    -- * General information
    , clientInfo
    , clientVersion
    -- * Concurrency
    , initLibrary
    , initThread
    , endThread
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (Exception, throw)
import Control.Monad (forM_, unless, when)
import Data.ByteString.Char8 ()
import Data.ByteString.Internal (ByteString, create, createAndTrim, memcpy)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.Word (Word, Word16, Word64)
import Database.MySQL.Base.C
import Database.MySQL.Base.Types
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CULong)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)

-- $license
--
-- /Important licensing note/: This library is BSD-licensed under the
-- terms of the MySQL FOSS License Exception
-- <http://www.mysql.com/about/legal/licensing/foss-exception/>.
--
-- Since this library links against the GPL-licensed @mysqlclient@
-- library, a non-open-source application that uses it /may/ be
-- subject to the terms of the GPL.

-- $mgmt
--
-- Our rules for managing 'Connection' and 'Result' values are
-- unfortunately complicated, thanks to MySQL's lifetime rules.
--
-- At the C @libmysqlclient@ level, a single @MYSQL@ connection may
-- cause multiple @MYSQL_RES@ result values to be created over the
-- course of multiple queries, but only one of these @MYSQL_RES@
-- values may be alive at a time.  The programmer is responsible for
-- knowing when to call @mysql_free_result@.
--
-- Meanwhile, up in Haskell-land, we'd like both 'Connection' and
-- 'Result' values to be managed either manually or automatically. In
-- particular, we want finalizers to tidy up after a messy programmer,
-- and we'd prefer it if people didn't need to be mindful of calling
-- @mysql_free_result@. This means that we must wrestle with the
-- lifetime rules. An obvious approach would be to use some monad and
-- type magic to enforce those rules, but then we'd end up with an
-- awkward API.
--
-- Instead, we allow 'Result' values to stay alive for arbitrarily
-- long times, while preserving the right to mark them as
-- invalid. When a @Result@ is marked invalid, its associated
-- @MYSQL_RES@ is freed, and can no longer be used.
--
-- Since all functions over @Result@ values are in the 'IO' monad, we
-- don't risk disrupting pure code by introducing this notion of
-- invalidity. If code tries to use an invalid @Result@, a
-- 'MySQLError' will be thrown. This should /not/ occur in normal
-- code, so there should be no need to use 'isResultValid' to test a
-- @Result@ for validity.
--
-- Each of the following functions will invalidate a 'Result':
--
-- * 'close'
--
-- * 'freeResult'
--
-- * 'nextResult'
--
-- * 'storeResult'
--
-- * 'useResult'
--
-- A 'Result' must be able to keep a 'Connection' alive so that a
-- streaming @Result@ constructed by 'useResult' can continue to pull
-- data from the server, but a @Connection@ must (a) be able to cause
-- the @MYSQL_RES@ behind a @Result@ to be deleted at a moment's notice,
-- while (b) not artificially prolonging the life of either the @Result@
-- or its @MYSQL_RES@.

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    , connectOptions :: [Option]
    , connectPath :: FilePath
    , connectSSL :: Maybe SSLInfo
    } deriving (Eq, Read, Show, Typeable)

data SSLInfo = SSLInfo {
      sslKey :: FilePath
    , sslCert :: FilePath
    , sslCA :: FilePath
    , sslCAPath :: FilePath
    , sslCiphers :: String -- ^ Comma-separated list of cipher names.
    } deriving (Eq, Read, Show, Typeable)

-- | The constructors of @MySQLError@ are not currently exported, but they
--   have a consistent set of field names which are exported.  These fields are:
--
--   >  errFunction :: String
--   >  errNumber   :: Int
--   >  errMessage  :: String
--
data MySQLError = ConnectionError {
      errFunction :: String
    , errNumber :: Int
    , errMessage :: String
    } | ResultError {
      errFunction :: String
    , errNumber :: Int
    , errMessage :: String
    } deriving (Eq, Show, Typeable)

instance Exception MySQLError

-- | Connection to a MySQL database.
data Connection = Connection {
      connFP :: ForeignPtr MYSQL
    , connClose :: IO ()
    , connResult :: IORef (Maybe (Weak Result))
    } deriving (Typeable)

-- | Result of a database query.
data Result = Result {
      resFP :: ForeignPtr MYSQL_RES
    , resFields :: {-# UNPACK #-} !Int
    , resConnection :: Connection
    , resValid :: IORef Bool
    , resFetchFields :: Ptr MYSQL_RES -> IO (Ptr Field)
    , resFetchRow :: Ptr MYSQL_RES -> IO MYSQL_ROW
    , resFetchLengths :: Ptr MYSQL_RES -> IO (Ptr CULong)
    , resFreeResult :: Ptr MYSQL_RES -> IO ()
    } | EmptyResult
  deriving (Typeable)

-- | A row cursor, used by 'rowSeek' and 'rowTell'.
newtype Row = Row MYSQL_ROW_OFFSET
  deriving (Typeable)

-- | Default information for setting up a connection.
--
-- Defaults are as follows:
--
-- * Server on @localhost@
--
-- * User @root@
--
-- * No password
--
-- * Database @test@
--
-- * Character set @utf8@
--
-- Use as in the following example:
--
-- > connect defaultConnectInfo { connectHost = "db.example.com" }
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo {
                       connectHost = "localhost"
                     , connectPort = 3306
                     , connectUser = "root"
                     , connectPassword = ""
                     , connectDatabase = "test"
                     , connectOptions = [CharsetName "utf8"]
                     , connectPath = ""
                     , connectSSL = Nothing
                     }

-- | Default (empty) information for setting up an SSL connection.
defaultSSLInfo :: SSLInfo
defaultSSLInfo = SSLInfo {
                   sslKey = ""
                 , sslCert = ""
                 , sslCA = ""
                 , sslCAPath = ""
                 , sslCiphers = ""
                 }

-- | Connect to a database.
connect :: ConnectInfo -> IO Connection
connect ConnectInfo{..} = do
  closed <- newIORef False
  ptr0 <- mysql_init nullPtr
  case connectSSL of
    Nothing -> return ()
    Just SSLInfo{..} -> withString sslKey $ \ckey ->
                         withString sslCert $ \ccert ->
                          withString sslCA $ \cca ->
                           withString sslCAPath $ \ccapath ->
                            withString sslCiphers $ \ccipher ->
                             mysql_ssl_set ptr0 ckey ccert cca ccapath ccipher
                             >> return ()
  forM_ connectOptions $ \opt -> do
    r <- mysql_options ptr0 opt
    unless (r == 0) $ connectionError_ "connect" ptr0
  let flags = foldl' (+) 0 . map toConnectFlag $ connectOptions
  ptr <- withString connectHost $ \chost ->
          withString connectUser $ \cuser ->
           withString connectPassword $ \cpass ->
            withString connectDatabase $ \cdb ->
             withString connectPath $ \cpath ->
               mysql_real_connect ptr0 chost cuser cpass cdb
                                  (fromIntegral connectPort) cpath flags
  when (ptr == nullPtr) $
    connectionError_ "connect" ptr0
  res <- newIORef Nothing
  let realClose = do
        cleanupConnResult res
        wasClosed <- atomicModifyIORef closed $ \prev -> (True, prev)
        unless wasClosed $ mysql_close ptr
  -- In general, the user of this library is responsible for dealing with thread
  -- safety. However, the programmer has no control over the OS thread
  -- finalizers are run from so we use 'runInBoundThread' and 'initThread' here.
  let myRunInBoundThread = if rtsSupportsBoundThreads then runInBoundThread else id
  fp <- newForeignPtr ptr (myRunInBoundThread $ initThread >> realClose)
  return Connection {
               connFP = fp
             , connClose = realClose
             , connResult = res
             }

-- | Delete the 'MYSQL_RES' behind a 'Result' immediately, and mark
-- the 'Result' as invalid.
cleanupConnResult :: IORef (Maybe (Weak Result)) -> IO ()
cleanupConnResult res = do
  prev <- readIORef res
  case prev of
    Nothing -> return ()
    Just w -> maybe (return ()) freeResult =<< deRefWeak w

-- | Close a connection, and mark any outstanding 'Result' as
-- invalid.
close :: Connection -> IO ()
close = connClose
{-# INLINE close #-}

ping :: Connection -> IO ()
ping conn = withConn conn $ \ptr -> mysql_ping ptr >>= check "ping" conn

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
  st <- mysql_stat ptr
  checkNull "serverStatus" conn st
  peekCString st

clientInfo :: String
clientInfo = unsafePerformIO $ peekCString mysql_get_client_info
{-# NOINLINE clientInfo #-}

clientVersion :: Word
clientVersion = fromIntegral mysql_get_client_version
{-# NOINLINE clientVersion #-}

-- | Turn autocommit on or off.
--
-- By default, MySQL runs with autocommit mode enabled. In this mode,
-- as soon as you modify a table, MySQL stores your modification
-- permanently.
autocommit :: Connection -> Bool -> IO ()
autocommit conn onOff = withConn conn $ \ptr ->
   mysql_autocommit ptr b >>= check "autocommit" conn
 where b = if onOff then 1 else 0

changeUser :: Connection -> String -> String -> Maybe String -> IO ()
changeUser conn user pass mdb =
  withCString user $ \cuser ->
   withCString pass $ \cpass ->
    withMaybeString mdb $ \cdb ->
     withConn conn $ \ptr ->
      mysql_change_user ptr cuser cpass cdb >>= check "changeUser" conn

selectDB :: Connection -> String -> IO ()
selectDB conn db =
  withCString db $ \cdb ->
    withConn conn $ \ptr ->
      mysql_select_db ptr cdb >>= check "selectDB" conn

query :: Connection -> ByteString -> IO ()
query conn q = withConn conn $ \ptr ->
  unsafeUseAsCStringLen q $ \(p,l) ->
  mysql_real_query ptr p (fromIntegral l) >>= check "query" conn

-- | Return the value generated for an @AUTO_INCREMENT@ column by the
-- previous @INSERT@ or @UPDATE@ statement.
--
-- See <http://dev.mysql.com/doc/refman/5.5/en/mysql-insert-id.html>
insertID :: Connection -> IO Word64
insertID conn = fromIntegral <$> (withConn conn $ mysql_insert_id)

-- | Return the number of fields (columns) in a result.
--
-- * If 'Left' 'Connection', returns the number of columns for the most
--   recent query on the connection.
--
-- * For 'Right' 'Result', returns the number of columns in each row
--   of this result.
--
-- The number of columns may legitimately be zero.
fieldCount :: Either Connection Result -> IO Int
fieldCount (Right EmptyResult) = return 0
fieldCount (Right res)         = return (resFields res)
fieldCount (Left conn)         =
    withConn conn $ fmap fromIntegral . mysql_field_count

affectedRows :: Connection -> IO Int64
affectedRows conn = withConn conn $ fmap fromIntegral . mysql_affected_rows

-- | Retrieve a complete result.
--
-- Any previous outstanding 'Result' is first marked as invalid.
storeResult :: Connection -> IO Result
storeResult = frobResult "storeResult" mysql_store_result
              mysql_fetch_fields_nonblock
              mysql_fetch_row_nonblock
              mysql_fetch_lengths_nonblock
              mysql_free_result_nonblock

-- | Initiate a row-by-row retrieval of a result.
--
-- Any previous outstanding 'Result' is first marked as invalid.
useResult :: Connection -> IO Result
useResult = frobResult "useResult" mysql_use_result
            mysql_fetch_fields
            mysql_fetch_row
            mysql_fetch_lengths
            mysql_free_result

frobResult :: String
           -> (Ptr MYSQL -> IO (Ptr MYSQL_RES))
           -> (Ptr MYSQL_RES -> IO (Ptr Field))
           -> (Ptr MYSQL_RES -> IO MYSQL_ROW)
           -> (Ptr MYSQL_RES -> IO (Ptr CULong))
           -> (Ptr MYSQL_RES -> IO ())
           -> Connection -> IO Result
frobResult func frob fetchFieldsFunc fetchRowFunc fetchLengthsFunc
           myFreeResult conn =
  withConn conn $ \ptr -> do
    cleanupConnResult (connResult conn)
    res <- frob ptr
    fields <- mysql_field_count ptr
    valid <- newIORef True
    if res == nullPtr
      then if fields == 0
           then return EmptyResult
           else connectionError func conn
      else do
        fp <- newForeignPtr res $ freeResult_ valid myFreeResult res
        let ret = Result {
                    resFP = fp
                  , resFields = fromIntegral fields
                  , resConnection = conn
                  , resValid = valid
                  , resFetchFields = fetchFieldsFunc
                  , resFetchRow = fetchRowFunc
                  , resFetchLengths = fetchLengthsFunc
                  , resFreeResult = myFreeResult
                  }
        weak <- mkWeakPtr ret (Just (freeResult_ valid myFreeResult res))
        writeIORef (connResult conn) (Just weak)
        return ret

-- | Immediately free the @MYSQL_RES@ value associated with this
-- 'Result', and mark the @Result@ as invalid.
freeResult :: Result -> IO ()
freeResult Result{..}  = withForeignPtr resFP $
                         freeResult_ resValid resFreeResult
freeResult EmptyResult = return ()

-- | Check whether a 'Result' is still valid, i.e. backed by a live
-- @MYSQL_RES@ value.
isResultValid :: Result -> IO Bool
isResultValid Result{..}  = readIORef resValid
isResultValid EmptyResult = return False

freeResult_ :: IORef Bool -> (Ptr MYSQL_RES -> IO ()) -> Ptr MYSQL_RES -> IO ()
freeResult_ valid free ptr = do
  wasValid <- atomicModifyIORef valid $ \prev -> (False, prev)
  when wasValid $ free ptr

fetchRow :: Result -> IO [Maybe ByteString]
fetchRow res@Result{..}  = withRes "fetchRow" res $ \ptr -> do
  rowPtr <- resFetchRow ptr
  if rowPtr == nullPtr
    then return []
    else do
      lenPtr <- resFetchLengths ptr
      checkNull "fetchRow" resConnection lenPtr
      let go len = withPtr $ \colPtr ->
                   create (fromIntegral len) $ \d ->
                   memcpy d (castPtr colPtr) (fromIntegral len)
      sequence =<< zipWith go <$> peekArray resFields lenPtr
                              <*> peekArray resFields rowPtr
fetchRow EmptyResult = return []

fetchFields :: Result -> IO [Field]
fetchFields res@Result{..} = withRes "fetchFields" res $ \ptr -> do
  peekArray resFields =<< resFetchFields ptr
fetchFields EmptyResult    = return []

dataSeek :: Result -> Int64 -> IO ()
dataSeek res row = withRes "dataSeek" res $ \ptr ->
  mysql_data_seek ptr (fromIntegral row)

rowTell :: Result -> IO Row
rowTell res = withRes "rowTell" res $ \ptr ->
  Row <$> mysql_row_tell ptr

rowSeek :: Result -> Row -> IO Row
rowSeek res (Row row) = withRes "rowSeek" res $ \ptr ->
  Row <$> mysql_row_seek ptr row

-- | Read the next statement result. Returns 'True' if another result
-- is available, 'False' otherwise.
--
-- This function marks the current 'Result' as invalid, if one exists.
nextResult :: Connection -> IO Bool
nextResult conn = withConn conn $ \ptr -> do
  cleanupConnResult (connResult conn)
  i <- mysql_next_result ptr
  case i of
    0  -> return True
    -1 -> return False
    _  -> connectionError "nextResult" conn

-- | Commit the current transaction.
commit :: Connection -> IO ()
commit conn = withConn conn $ \ptr ->
              mysql_commit ptr >>= check "commit" conn

-- | Roll back the current transaction.
rollback :: Connection -> IO ()
rollback conn = withConn conn $ \ptr ->
                mysql_rollback ptr >>= check "rollback" conn

escape :: Connection -> ByteString -> IO ByteString
escape conn bs = withConn conn $ \ptr ->
  unsafeUseAsCStringLen bs $ \(p,l) ->
    createAndTrim (l*2 + 1) $ \to ->
      fromIntegral <$> mysql_real_escape_string ptr (castPtr to) p
                                                (fromIntegral l)

withConn :: Connection -> (Ptr MYSQL -> IO a) -> IO a
withConn conn = withForeignPtr (connFP conn)

-- | Call @mysql_library_init@
--
-- A single-threaded program can rely on an implicit initialisation done
-- when making the first connection, but a multi-threaded one should call
-- 'initLibrary' separately, and it should be done before other threads
-- might call into this library, since this function is not thread-safe.
-- See <https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell>
-- and <https://dev.mysql.com/doc/refman/5.7/en/c-api-threaded-clients.html>
-- for details.
initLibrary :: IO ()
initLibrary = do
  r <- mysql_library_init 0 nullPtr nullPtr
  if r == 0
    then return ()
    else throw $ ConnectionError "initLibrary" (-1)
      "mysql_library_init failed"

-- | Call @mysql_thread_init@
--
-- Again a single-threaded program does not need to call this explicitly.  Even
-- in a multi-threaded one, if each connection is made, used, and destroyed
-- in a single thread, it is sufficient to rely on the 'connect' call to do
-- an implicit thread initialisation.  But in other cases, for example when
-- using a connection pool, each thread requires explicit initialisation.
-- See <https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell>
-- and <https://dev.mysql.com/doc/refman/5.7/en/c-api-threaded-clients.html>
-- for details.
initThread :: IO ()
initThread = do
  r <- mysql_thread_init
  if r == 0
    then return ()
    else throw $ ConnectionError "initThread" (-1)
      "mysql_thread_init failed"

-- | Call @mysql_thread_end@
--
-- This is needed at thread exit to avoid a memory leak, except when using
-- a non-debug build of at least version 5.7.9 of the MySQL library.
-- See <https://dev.mysql.com/doc/refman/5.7/en/mysql-thread-end.html>.
-- The threads in question are the /OS threads/, so calling this function
-- is likely to be important when using large numbers of bound threads (see
-- "Control.Concurrent").  Unbound threads - those created with 'forkIO' and
-- friends - share a small number of OS threads, so in those it is hard to
-- call this function safely, and there is little benefit in doing so, but in
-- any case using this library in unbound threads is not recommended  (see
-- <https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell>).
endThread :: IO ()
endThread = mysql_thread_end

withRes :: String -> Result -> (Ptr MYSQL_RES -> IO a) -> IO a
withRes func res act = do
  valid <- readIORef (resValid res)
  unless valid . throw $ ResultError func 0 "result is no longer usable"
  withForeignPtr (resFP res) act

withString :: String -> (CString -> IO a) -> IO a
withString [] act = act nullPtr
withString xs act = withCString xs act

withMaybeString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeString Nothing act = act nullPtr
withMaybeString (Just xs) act = withCString xs act

check :: (Eq a, Num a) => String -> Connection -> a -> IO ()
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
