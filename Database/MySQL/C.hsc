{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Database.MySQL.C
    (
    -- * Types
      MYSQL
    , MYSQL_STMT
    -- * Connection management
    , mysql_init
    , mysql_real_connect
    , mysql_close
    , mysql_ping
    , mysql_thread_id
    -- * Error handling
    , mysql_errno
    , mysql_error
    , mysql_stmt_errno
    , mysql_stmt_error
    -- * Support functions
    , withRTSSignalsBlocked
    ) where

#include "mysql.h"
#include <signal.h>

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (finally)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

data MYSQL
data MYSQL_STMT

-- | Execute an 'IO' action with signals used by GHC's runtime signals
-- blocked.  The @mysqlclient@ C library does not correctly restart
-- system calls if they are interrupted by signals, so many MySQL API
-- calls can unexpectedly fail when called from a Haskell application.
-- This is most likely to occur if you are linking against GHC's
-- threaded runtime (using the @-threaded@ option).
--
-- This function blocks @SIGALRM@ and @SIGVTALRM@, runs your action,
-- then unblocks those signals.  If you have a series of HDBC calls
-- that may block for a period of time, it may be wise to wrap them in
-- this action.  Blocking and unblocking signals is cheap, but not
-- free.
--
-- Here is an example of an exception that could be avoided by
-- temporarily blocking GHC's runtime signals:
--
-- >  SqlError {
-- >    seState = "", 
-- >    seNativeError = 2003, 
-- >    seErrorMsg = "Can't connect to MySQL server on 'localhost' (4)"
-- >  }
withRTSSignalsBlocked :: IO a -> IO a
withRTSSignalsBlocked act
    | not rtsSupportsBoundThreads = act
    | otherwise = runInBoundThread . alloca $ \set -> do
  sigemptyset set
  sigaddset set (#const SIGALRM)
  sigaddset set (#const SIGVTALRM)
  pthread_sigmask (#const SIG_BLOCK) set nullPtr
  act `finally` pthread_sigmask (#const SIG_UNBLOCK) set nullPtr

data SigSet

instance Storable SigSet where
    sizeOf    _ = #{size sigset_t}
    alignment _ = alignment (undefined :: Ptr CInt)

foreign import ccall unsafe "signal.h sigaddset" sigaddset
    :: Ptr SigSet -> CInt -> IO ()

foreign import ccall unsafe "signal.h sigemptyset" sigemptyset
    :: Ptr SigSet -> IO ()

foreign import ccall unsafe "signal.h pthread_sigmask" pthread_sigmask
    :: CInt -> Ptr SigSet -> Ptr SigSet -> IO ()

foreign import ccall safe mysql_init
    :: Ptr MYSQL                -- ^ should usually be 'nullPtr'
    -> IO (Ptr MYSQL)

foreign import ccall unsafe mysql_real_connect
    :: Ptr MYSQL -- ^ context (from 'mysql_init')
    -> CString   -- ^ hostname
    -> CString   -- ^ username
    -> CString   -- ^ password
    -> CString   -- ^ database
    -> CInt      -- ^ port
    -> CString   -- ^ unix socket
    -> IO (Ptr MYSQL)

foreign import ccall unsafe mysql_close
    :: Ptr MYSQL -> IO ()

foreign import ccall unsafe mysql_ping
    :: Ptr MYSQL -> IO CInt

foreign import ccall unsafe mysql_thread_id
    :: Ptr MYSQL -> IO CULong

foreign import ccall unsafe mysql_errno
    :: Ptr MYSQL -> IO CInt

foreign import ccall unsafe mysql_error
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe mysql_stmt_errno
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall unsafe mysql_stmt_error
    :: Ptr MYSQL_STMT -> IO CString
