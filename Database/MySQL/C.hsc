{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}

module Database.MySQL.C
    (
    -- * Types
    -- * High-level types
      Type(..)
    , Field(..)
    , FieldFlag
    , FieldFlags
    -- * Low-level types
    , MYSQL
    , MYSQL_RES
    , MYSQL_ROW
    , MyBool
    -- * Connection management
    , mysql_init
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
    , mysql_get_host_info
    , mysql_get_proto_info
    , mysql_character_set_name
    , mysql_get_ssl_cipher
    , mysql_stat
    -- * Querying
    , mysql_real_query
    -- ** Escaping
    , mysql_real_escape_string
    -- ** Results
    , mysql_field_count
    , mysql_affected_rows
    , mysql_store_result
    , mysql_use_result
    , mysql_fetch_lengths
    , mysql_fetch_row
    -- * Working with results
    , mysql_free_result
    , mysql_fetch_fields
    -- * Field flags
    , hasAllFlags
    , flagNotNull
    , flagPrimaryKey
    , flagUniqueKey
    , flagMultipleKey
    , flagUnsigned
    , flagZeroFill
    , flagBinary
    , flagAutoIncrement
    , flagNumeric
    , flagNoDefaultValue
    -- * General information
    , mysql_get_client_info
    , mysql_get_client_version
    -- * Error handling
    , mysql_errno
    , mysql_error
    -- * Support functions
    , withRTSSignalsBlocked
    ) where

#include "mysql.h"
#include <signal.h>

import Data.Monoid
import Data.Bits
import Data.List
import Control.Applicative
import Data.Maybe
import qualified Data.IntMap as IntMap
import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (finally)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable
import Data.Typeable (Typeable)
import Data.ByteString hiding (intercalate)
import Data.ByteString.Internal
import Data.Word

data MYSQL
data MYSQL_RES
type MYSQL_ROW = Ptr (Ptr CChar)
type MyBool = CChar

-- | Column types supported by MySQL.
data Type = Decimal
          | Tiny
          | Short
          | Long
          | Float
          | Double
          | Null
          | Timestamp
          | LongLong
          | Int24
          | Date
          | Time
          | DateTime
          | Year
          | NewDate
          | VarChar
          | Bit
          | NewDecimal
          | Enum
          | Set
          | TinyBlob
          | MediumBlob
          | LongBlob
          | Blob
          | VarString
          | String
          | Geometry
            deriving (Enum, Eq, Show, Typeable)

toType :: CInt -> Type
toType v = IntMap.findWithDefault oops (fromIntegral v) typeMap
  where
    oops = error $ "Database.MySQL: unknown field type " ++ show v
    typeMap = IntMap.fromList [
               ((#const MYSQL_TYPE_DECIMAL), Decimal),
               ((#const MYSQL_TYPE_TINY), Tiny),
               ((#const MYSQL_TYPE_SHORT), Short),
               ((#const MYSQL_TYPE_LONG), Long),
               ((#const MYSQL_TYPE_FLOAT), Float),
               ((#const MYSQL_TYPE_DOUBLE), Double),
               ((#const MYSQL_TYPE_NULL), Null),
               ((#const MYSQL_TYPE_TIMESTAMP), Timestamp),
               ((#const MYSQL_TYPE_LONGLONG), LongLong),
               ((#const MYSQL_TYPE_DATE), Date),
               ((#const MYSQL_TYPE_TIME), Time),
               ((#const MYSQL_TYPE_DATETIME), DateTime),
               ((#const MYSQL_TYPE_YEAR), Year),
               ((#const MYSQL_TYPE_NEWDATE), NewDate),
               ((#const MYSQL_TYPE_VARCHAR), VarChar),
               ((#const MYSQL_TYPE_BIT), Bit),
               ((#const MYSQL_TYPE_NEWDECIMAL), NewDecimal),
               ((#const MYSQL_TYPE_ENUM), Enum),
               ((#const MYSQL_TYPE_SET), Set),
               ((#const MYSQL_TYPE_TINY_BLOB), TinyBlob),
               ((#const MYSQL_TYPE_MEDIUM_BLOB), MediumBlob),
               ((#const MYSQL_TYPE_LONG_BLOB), LongBlob),
               ((#const MYSQL_TYPE_BLOB), Blob),
               ((#const MYSQL_TYPE_VAR_STRING), VarString),
               ((#const MYSQL_TYPE_STRING), String),
               ((#const MYSQL_TYPE_GEOMETRY), Geometry)
              ]

-- | A description of a field (column) of a table.
data Field = Field {
      fieldName :: ByteString   -- ^ Name of column.
    , fieldOrigName :: ByteString -- ^ Original column name, if an alias.
    , fieldTable :: ByteString -- ^ Table of column, if column was a field.
    , fieldOrigTable :: ByteString -- ^ Original table name, if table was an alias.
    , fieldDB :: ByteString        -- ^ Database for table.
    , fieldCatalog :: ByteString   -- ^ Catalog for table.
    , fieldDefault :: Maybe ByteString   -- ^ Default value.
    , fieldLength :: Word          -- ^ Width of column (create length).
    , fieldMaxLength :: Word    -- ^ Maximum width for selected set.
    , fieldFlags :: FieldFlags        -- ^ Div flags.
    , fieldDecimals :: Word -- ^ Number of decimals in field.
    , fieldCharSet :: Word -- ^ Character set number.
    , fieldType :: Type
    } deriving (Eq, Show, Typeable)

newtype FieldFlags = FieldFlags CUInt
    deriving (Eq, Typeable)

instance Show FieldFlags where
    show f = '[' : z ++ "]"
      where z = intercalate "," . catMaybes $ [
                          flagNotNull ??? "flagNotNull"
                        , flagPrimaryKey ??? "flagPrimaryKey"
                        , flagUniqueKey ??? "flagUniqueKey"
                        , flagMultipleKey ??? "flagMultipleKey"
                        , flagUnsigned ??? "flagUnsigned"
                        , flagZeroFill ??? "flagZeroFill"
                        , flagBinary ??? "flagBinary"
                        , flagAutoIncrement ??? "flagAutoIncrement"
                        , flagNumeric ??? "flagNumeric"
                        , flagNoDefaultValue ??? "flagNoDefaultValue"
                        ]
            flag ??? name | f `hasAllFlags` flag = Just name
                          | otherwise            = Nothing

type FieldFlag = FieldFlags

instance Monoid FieldFlags where
    mempty = FieldFlags 0
    {-# INLINE mempty #-}
    mappend (FieldFlags a) (FieldFlags b) = FieldFlags (a .|. b)
    {-# INLINE mappend #-}

flagNotNull, flagPrimaryKey, flagUniqueKey, flagMultipleKey :: FieldFlag
flagNotNull = FieldFlags #const NOT_NULL_FLAG
flagPrimaryKey = FieldFlags #const PRI_KEY_FLAG
flagUniqueKey = FieldFlags #const UNIQUE_KEY_FLAG
flagMultipleKey = FieldFlags #const MULTIPLE_KEY_FLAG

flagUnsigned, flagZeroFill, flagBinary, flagAutoIncrement :: FieldFlag
flagUnsigned = FieldFlags #const UNSIGNED_FLAG
flagZeroFill = FieldFlags #const ZEROFILL_FLAG
flagBinary = FieldFlags #const BINARY_FLAG
flagAutoIncrement = FieldFlags #const AUTO_INCREMENT_FLAG

flagNumeric, flagNoDefaultValue :: FieldFlag
flagNumeric = FieldFlags #const NUM_FLAG
flagNoDefaultValue = FieldFlags #const NO_DEFAULT_VALUE_FLAG

hasAllFlags :: FieldFlags -> FieldFlags -> Bool
FieldFlags a `hasAllFlags` FieldFlags b = a .&. b == b
{-# INLINE hasAllFlags #-}

peekField :: Ptr Field -> IO Field
peekField ptr = do
  flags <- FieldFlags <$> (#peek MYSQL_FIELD, flags) ptr
  Field
   <$> peekS ((#peek MYSQL_FIELD, name)) ((#peek MYSQL_FIELD, name_length))
   <*> peekS ((#peek MYSQL_FIELD, org_name)) ((#peek MYSQL_FIELD, org_name_length))
   <*> peekS ((#peek MYSQL_FIELD, table)) ((#peek MYSQL_FIELD, table_length))
   <*> peekS ((#peek MYSQL_FIELD, org_table)) ((#peek MYSQL_FIELD, org_table_length))
   <*> peekS ((#peek MYSQL_FIELD, db)) ((#peek MYSQL_FIELD, db_length))
   <*> peekS ((#peek MYSQL_FIELD, catalog)) ((#peek MYSQL_FIELD, catalog_length))
   <*> (if flags `hasAllFlags` flagNoDefaultValue
       then pure Nothing
       else Just <$> peekS ((#peek MYSQL_FIELD, def)) ((#peek MYSQL_FIELD, def_length)))
   <*> (uint <$> (#peek MYSQL_FIELD, length) ptr)
   <*> (uint <$> (#peek MYSQL_FIELD, max_length) ptr)
   <*> pure flags
   <*> (uint <$> (#peek MYSQL_FIELD, decimals) ptr)
   <*> (uint <$> (#peek MYSQL_FIELD, charsetnr) ptr)
   <*> (toType <$> (#peek MYSQL_FIELD, type) ptr)
 where
   uint = fromIntegral :: CUInt -> Word
   peekS :: (Ptr Field -> IO (Ptr Word8)) -> (Ptr Field -> IO CUInt)
         -> IO ByteString
   peekS peekPtr peekLen = do
     p <- peekPtr ptr
     l <- peekLen ptr
     create (fromIntegral l) $ \d -> memcpy d p (fromIntegral l)

instance Storable Field where
    sizeOf _    = #{size MYSQL_FIELD}
    alignment _ = alignment (undefined :: Ptr CChar)
    peek = peekField

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
    | otherwise = runInBoundThread . withForeignPtr rtsSignals $ \set -> do
  pthread_sigmask (#const SIG_BLOCK) set nullPtr
  act `finally` pthread_sigmask (#const SIG_UNBLOCK) set nullPtr

rtsSignals :: ForeignPtr SigSet
rtsSignals = unsafePerformIO $ do
               fp <- mallocForeignPtr
               withForeignPtr fp $ \set -> do
                 sigemptyset set
                 sigaddset set (#const SIGALRM)
                 sigaddset set (#const SIGVTALRM)
               return fp
{-# NOINLINE rtsSignals #-}

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

foreign import ccall safe mysql_thread_id
    :: Ptr MYSQL -> IO CULong

foreign import ccall unsafe mysql_autocommit
    :: Ptr MYSQL -> MyBool -> IO MyBool

foreign import ccall unsafe mysql_change_user
    :: Ptr MYSQL
    -> CString                  -- ^ user
    -> CString                  -- ^ password
    -> CString                  -- ^ database
    -> IO MyBool

foreign import ccall unsafe mysql_select_db
    :: Ptr MYSQL
    -> CString
    -> IO CInt

foreign import ccall safe mysql_get_server_info
    :: Ptr MYSQL -> IO CString

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

foreign import ccall unsafe mysql_stat
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe mysql_real_query
    :: Ptr MYSQL -> CString -> CULong -> IO CInt

foreign import ccall safe mysql_field_count
    :: Ptr MYSQL -> IO CUInt

foreign import ccall safe mysql_affected_rows
    :: Ptr MYSQL -> IO CULLong

foreign import ccall unsafe mysql_store_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall unsafe mysql_use_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall unsafe mysql_free_result
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall unsafe mysql_fetch_fields
    :: Ptr MYSQL_RES -> IO (Ptr Field)

foreign import ccall unsafe mysql_fetch_row
    :: Ptr MYSQL_RES -> IO MYSQL_ROW

foreign import ccall unsafe mysql_fetch_lengths
    :: Ptr MYSQL_RES -> IO (Ptr CULong)

foreign import ccall safe mysql_real_escape_string
    :: Ptr MYSQL -> CString -> CString -> CULong -> IO CULong

foreign import ccall safe mysql_get_client_info :: CString

foreign import ccall safe mysql_get_client_version :: CULong

foreign import ccall safe mysql_errno
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe mysql_error
    :: Ptr MYSQL -> IO CString
