{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module:      Database.MySQL.Base.C
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- Direct bindings to the C @mysqlclient@ API.
module Database.MySQL.Base.C
    (
    -- * Connection management
      mysql_init
    , mysql_options
    , mysql_ssl_set
    , mysql_real_connect
    , mysql_close
    , mysql_ping
    , mysql_autocommit
    , mysql_change_user
    , mysql_select_db
    , mysql_set_character_set
    -- ** Connection information
    , mysql_thread_id
    , mysql_get_server_info
    , mysql_get_server_version
    , mysql_get_host_info
    , mysql_get_proto_info
    , mysql_character_set_name
    , mysql_get_ssl_cipher
    , mysql_stat
    -- * Querying
    , mysql_real_query
    , mysql_insert_id
    -- ** Escaping
    , mysql_real_escape_string
    -- ** Results
    , mysql_field_count
    , mysql_affected_rows
    , mysql_store_result
    , mysql_use_result
    , mysql_fetch_lengths
    , mysql_fetch_lengths_nonblock
    , mysql_fetch_row
    , mysql_fetch_row_nonblock
    -- * Working with results
    , mysql_free_result
    , mysql_free_result_nonblock
    , mysql_num_fields
    , mysql_fetch_fields
    , mysql_fetch_fields_nonblock
    , mysql_data_seek
    , mysql_row_seek
    , mysql_row_tell
    -- ** Multiple results
    , mysql_next_result
    -- * Transactions
    , mysql_commit
    , mysql_rollback
    -- * General information
    , mysql_get_client_info
    , mysql_get_client_version
    -- * Error handling
    , mysql_errno
    , mysql_error
    -- * Concurrency
    , mysql_library_init
    , mysql_thread_init
    , mysql_thread_end


    , mysql_stmt_init
    , mysql_stmt_close
    , mysql_stmt_prepare
    , mysql_stmt_bind_param
    , mysql_stmt_bind_result
    , mysql_stmt_execute
    , mysql_stmt_fetch
    , mysql_stmt_store_result
    , mysql_stmt_result_metadata
    , mysql_stmt_free_result
    , mysql_stmt_error
    ) where

#include "mysql_signals.h"
#include "mysql.h"

import Data.ByteString (useAsCString)
import Database.MySQL.Base.Types
import Foreign.C.String (CString, withCString)
##if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CChar(..), CInt(..), CUInt(..), CULLong(..), CULong(..))
##else
import Foreign.C.Types (CInt, CUInt, CULLong, CULong)
##endif
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall safe mysql_init
    :: Ptr MYSQL                -- ^ should usually be 'nullPtr'
    -> IO (Ptr MYSQL)

mysql_options :: Ptr MYSQL -> Option -> IO CInt
mysql_options ptr opt =
    case opt of
      ConnectTimeout secs ->
        withIntegral secs $ go (#const MYSQL_OPT_CONNECT_TIMEOUT)
      Compress ->
        go (#const MYSQL_OPT_COMPRESS) nullPtr
      NamedPipe ->
        go (#const MYSQL_OPT_NAMED_PIPE) nullPtr
      InitCommand cmd ->
        useAsCString cmd $ go (#const MYSQL_INIT_COMMAND)
      ReadDefaultFile path ->
        withCString path $ go (#const MYSQL_READ_DEFAULT_FILE)
      ReadDefaultGroup group ->
        useAsCString group $ go (#const MYSQL_READ_DEFAULT_GROUP)
      CharsetDir path ->
        withCString path $ go (#const MYSQL_SET_CHARSET_DIR)
      CharsetName cs ->
        withCString cs $ go (#const MYSQL_SET_CHARSET_NAME)
      LocalInFile b ->
        withBool b $ go (#const MYSQL_OPT_LOCAL_INFILE)
      Protocol proto ->
        withIntegral (fromEnum proto) $ go (#const MYSQL_OPT_PROTOCOL)
      SharedMemoryBaseName name ->
        useAsCString name $ go (#const MYSQL_SHARED_MEMORY_BASE_NAME)
      ReadTimeout secs ->
        withIntegral secs $ go (#const MYSQL_OPT_READ_TIMEOUT)
      WriteTimeout secs ->
        withIntegral secs $ go (#const MYSQL_OPT_WRITE_TIMEOUT)
#if !defined(MARIADB_BASE_VERSION) && MYSQL_VERSION_ID >= 80000
#else
      UseRemoteConnection ->
        go (#const MYSQL_OPT_USE_REMOTE_CONNECTION) nullPtr
      UseEmbeddedConnection ->
        go (#const MYSQL_OPT_USE_EMBEDDED_CONNECTION) nullPtr
      GuessConnection ->
        go (#const MYSQL_OPT_GUESS_CONNECTION) nullPtr
      ClientIP ip ->
        useAsCString ip $ go (#const MYSQL_SET_CLIENT_IP)
      SecureAuth b ->
        withBool b $ go (#const MYSQL_SECURE_AUTH)
#endif
      ReportDataTruncation b ->
        withBool b $ go (#const MYSQL_REPORT_DATA_TRUNCATION)
      Reconnect b ->
        withBool b $ go (#const MYSQL_OPT_RECONNECT)
#if !defined(MARIADB_BASE_VERSION) && MYSQL_VERSION_ID >= 80000
#else
      SSLVerifyServerCert b ->
        withBool b $ go (#const MYSQL_OPT_SSL_VERIFY_SERVER_CERT)
#endif
      -- Other options are accepted by mysql_real_connect, so ignore them.
      _ -> return 0
  where
    go = mysql_options_ ptr
    withBool b = with (if b then 1 else 0 :: CUInt)
    withIntegral i = with (fromIntegral i :: CUInt)

foreign import ccall safe "mysql.h mysql_options" mysql_options_
    :: Ptr MYSQL -> CInt -> Ptr a -> IO CInt

foreign import ccall safe "mysql_signals.h _hs_mysql_real_connect"
        mysql_real_connect
    :: Ptr MYSQL -- ^ Context (from 'mysql_init').
    -> CString   -- ^ Host name.
    -> CString   -- ^ User name.
    -> CString   -- ^ Password.
    -> CString   -- ^ Database.
    -> CInt      -- ^ Port.
    -> CString   -- ^ Unix socket.
    -> CULong    -- ^ Flags.
    -> IO (Ptr MYSQL)

foreign import ccall safe mysql_ssl_set
    :: Ptr MYSQL
    -> CString                  -- ^ Key.
    -> CString                  -- ^ Cert.
    -> CString                  -- ^ CA.
    -> CString                  -- ^ CA path.
    -> CString                  -- ^ Ciphers.
    -> IO MyBool

foreign import ccall safe "mysql_signals.h _hs_mysql_close" mysql_close
    :: Ptr MYSQL -> IO ()

foreign import ccall safe "mysql_signals.h _hs_mysql_ping" mysql_ping
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe mysql_thread_id
    :: Ptr MYSQL -> IO CULong

foreign import ccall safe "mysql_signals.h _hs_mysql_autocommit" mysql_autocommit
    :: Ptr MYSQL -> MyBool -> IO MyBool

foreign import ccall safe "mysql_signals.h _hs_mysql_change_user" mysql_change_user
    :: Ptr MYSQL
    -> CString                  -- ^ user
    -> CString                  -- ^ password
    -> CString                  -- ^ database
    -> IO MyBool

foreign import ccall safe "mysql_signals.h _hs_mysql_select_db" mysql_select_db
    :: Ptr MYSQL
    -> CString
    -> IO CInt

foreign import ccall safe mysql_get_server_info
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_get_server_version
    :: Ptr MYSQL -> IO CULong

foreign import ccall safe mysql_get_host_info
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_get_proto_info
    :: Ptr MYSQL -> IO CUInt

foreign import ccall safe mysql_character_set_name
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_set_character_set
    :: Ptr MYSQL -> CString -> IO CInt

foreign import ccall safe mysql_get_ssl_cipher
    :: Ptr MYSQL -> IO CString

foreign import ccall safe "mysql_signals.h _hs_mysql_stat" mysql_stat
    :: Ptr MYSQL -> IO CString

foreign import ccall safe "mysql_signals.h _hs_mysql_real_query" mysql_real_query
    :: Ptr MYSQL -> CString -> CULong -> IO CInt

foreign import ccall safe mysql_insert_id
    :: Ptr MYSQL -> IO CULLong

foreign import ccall safe mysql_field_count
    :: Ptr MYSQL -> IO CUInt

foreign import ccall safe mysql_affected_rows
    :: Ptr MYSQL -> IO CULLong

foreign import ccall safe "mysql_signals.h _hs_mysql_store_result" mysql_store_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall safe "mysql_signals.h _hs_mysql_use_result"  mysql_use_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall safe "mysql_signals.h _hs_mysql_free_result" mysql_free_result
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall safe "mysql.h mysql_free_result" mysql_free_result_nonblock
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall safe mysql_num_fields
    :: Ptr MYSQL_RES -> IO CUInt

foreign import ccall safe mysql_fetch_fields
    :: Ptr MYSQL_RES -> IO (Ptr Field)

foreign import ccall safe "mysql.h mysql_fetch_fields" mysql_fetch_fields_nonblock
    :: Ptr MYSQL_RES -> IO (Ptr Field)

foreign import ccall safe mysql_data_seek
    :: Ptr MYSQL_RES -> CULLong -> IO ()

foreign import ccall safe mysql_row_seek
    :: Ptr MYSQL_RES -> MYSQL_ROW_OFFSET -> IO MYSQL_ROW_OFFSET

foreign import ccall safe mysql_row_tell
    :: Ptr MYSQL_RES -> IO MYSQL_ROW_OFFSET

foreign import ccall safe "mysql_signals.h _hs_mysql_next_result" mysql_next_result
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe "mysql_signals.h _hs_mysql_commit" mysql_commit
    :: Ptr MYSQL -> IO MyBool

foreign import ccall safe "mysql_signals.h _hs_mysql_rollback" mysql_rollback
    :: Ptr MYSQL -> IO MyBool

foreign import ccall safe "mysql_signals.h _hs_mysql_fetch_row" mysql_fetch_row
    :: Ptr MYSQL_RES -> IO MYSQL_ROW

foreign import ccall safe "mysql.h mysql_fetch_row" mysql_fetch_row_nonblock
    :: Ptr MYSQL_RES -> IO MYSQL_ROW

foreign import ccall safe mysql_fetch_lengths
    :: Ptr MYSQL_RES -> IO (Ptr CULong)

foreign import ccall safe "mysql.h mysql_fetch_lengths" mysql_fetch_lengths_nonblock
    :: Ptr MYSQL_RES -> IO (Ptr CULong)

foreign import ccall safe mysql_real_escape_string
    :: Ptr MYSQL -> CString -> CString -> CULong -> IO CULong

foreign import ccall safe mysql_get_client_info :: CString

foreign import ccall safe mysql_get_client_version :: CULong

foreign import ccall safe mysql_errno
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe mysql_error
    :: Ptr MYSQL -> IO CString

foreign import ccall safe "mysql.h mysql_server_init" mysql_library_init
    :: CInt -> Ptr (Ptr Char) -> Ptr (Ptr Char) -> IO CInt

foreign import ccall safe "mysql.h" mysql_thread_init
    :: IO MyBool

foreign import ccall safe "mysql.h" mysql_thread_end
    :: IO ()


-- Prepared Statements
foreign import ccall safe mysql_stmt_init
    :: Ptr MYSQL -> IO (Ptr MYSQL_STMT)

foreign import ccall safe mysql_stmt_close
    :: Ptr MYSQL_STMT -> IO MyBool

foreign import ccall safe mysql_stmt_bind_param
    :: Ptr MYSQL_STMT -> Ptr MYSQL_BIND -> IO MyBool

foreign import ccall safe mysql_stmt_bind_result
    :: Ptr MYSQL_STMT -> Ptr MYSQL_BIND -> IO MyBool

foreign import ccall safe mysql_stmt_errno
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall safe mysql_stmt_error
    :: Ptr MYSQL_STMT -> IO CString

foreign import ccall safe mysql_stmt_prepare
    :: Ptr MYSQL_STMT -> CString -> CULong -> IO CInt

foreign import ccall safe mysql_stmt_execute
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall safe mysql_stmt_fetch
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall safe mysql_stmt_store_result
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall safe mysql_stmt_insert_id
    :: Ptr MYSQL_STMT -> IO CULLong

foreign import ccall safe mysql_stmt_field_count
    :: Ptr MYSQL_STMT -> IO CUInt

foreign import ccall safe mysql_stmt_affected_rows
    :: Ptr MYSQL_STMT -> IO CULLong

foreign import ccall safe mysql_stmt_result_metadata
    :: Ptr MYSQL_STMT -> IO (Ptr MYSQL_RES) 

foreign import ccall safe mysql_stmt_free_result
    :: Ptr MYSQL_STMT -> IO MyBool
