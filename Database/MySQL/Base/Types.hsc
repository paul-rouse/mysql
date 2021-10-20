{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module:      Database.MySQL.Base.C
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Paul Rouse <pyr@doynton.org>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with the direct bindings to the C @mysqlclient@
-- API.

module Database.MySQL.Base.Types
    (
    -- * Types
    -- * High-level types
      Type(..)
    , Seconds
    , Protocol(..)
    , Option(..)
    , Field(..)
    , FieldFlag
    , FieldFlags
    -- * Low-level types
    , MYSQL
    , MYSQL_RES
    , MYSQL_ROW
    , MYSQL_ROWS
    , MYSQL_ROW_OFFSET
    , MYSQL_STMT
    , MYSQL_BIND(..)
    , MYSQL_TIME(..)
    , MyBool
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
    -- * Connect flags
    , toConnectFlag
    ) where

#include "mysql.h"

import Control.Applicative ((<$>), (<*>), pure)
import Data.Bits ((.|.), (.&.))
import Data.ByteString hiding (intercalate)
import Data.ByteString.Internal (create, memcpy)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Typeable (Typeable)
import Data.Word (Word, Word8)
import Foreign.C.Types (CChar, CInt, CUInt, CULong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), peekByteOff)
import qualified Foreign  as Foreign
import qualified Data.IntMap as IntMap

data MYSQL
data MYSQL_RES
data MYSQL_STMT
data MYSQL_ROWS
type MYSQL_ROW = Ptr (Ptr CChar)
type MYSQL_ROW_OFFSET = Ptr MYSQL_ROWS
type MyBool = CChar

-- "mysql.h" defines the `MYSQL_TYPE_...` symbols as values of an enumeration,
-- not as preprocessor symbols.  Therefore we can't test for the presence of
-- `MYSQL_TYPE_JSON` using `#ifdef` or `#if defined()`, yet it is not available
-- in all versions of MySQL and MariaDB.  Although this is very unsatisfactory,
-- we have little alternative but to define it here.
--
mysql_type_json :: Int
mysql_type_json = 245

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
          | Json
            deriving (Enum, Eq, Show, Typeable)

fromType :: Type -> CInt
fromType t =
    case t of
      Decimal -> #const MYSQL_TYPE_DECIMAL
      Tiny -> #const MYSQL_TYPE_TINY
      Short -> #const MYSQL_TYPE_SHORT
      Int24 ->  #const MYSQL_TYPE_INT24
      Long ->  #const MYSQL_TYPE_LONG
      Float ->  #const MYSQL_TYPE_FLOAT
      Double ->  #const MYSQL_TYPE_DOUBLE
      Null ->  #const MYSQL_TYPE_NULL
      Timestamp -> #const MYSQL_TYPE_TIMESTAMP
      LongLong -> #const MYSQL_TYPE_LONGLONG
      Date -> #const MYSQL_TYPE_DATE
      Time -> #const MYSQL_TYPE_TIME
      DateTime -> #const MYSQL_TYPE_DATETIME 
      Year -> #const MYSQL_TYPE_YEAR
      NewDate -> #const MYSQL_TYPE_NEWDATE
      VarChar -> #const MYSQL_TYPE_VARCHAR
      Bit -> #const MYSQL_TYPE_BIT
      NewDecimal -> #const MYSQL_TYPE_NEWDECIMAL
      Enum -> #const MYSQL_TYPE_ENUM
      Set -> #const MYSQL_TYPE_SET
      TinyBlob -> #const MYSQL_TYPE_TINY_BLOB
      MediumBlob -> #const MYSQL_TYPE_MEDIUM_BLOB
      LongBlob -> #const MYSQL_TYPE_LONG_BLOB
      Blob -> #const MYSQL_TYPE_BLOB
      VarString -> #const MYSQL_TYPE_VAR_STRING
      String -> #const MYSQL_TYPE_STRING
      Geometry -> #const MYSQL_TYPE_GEOMETRY
      Json -> mysql_type_json
      
      
toType :: CInt -> Type
toType v = IntMap.findWithDefault oops (fromIntegral v) typeMap
  where
    oops = error $ "Database.MySQL: unknown field type " ++ show v
    typeMap = IntMap.fromList
      [ ((#const MYSQL_TYPE_DECIMAL), Decimal)
      , ((#const MYSQL_TYPE_TINY), Tiny)
      , ((#const MYSQL_TYPE_SHORT), Short)
      , ((#const MYSQL_TYPE_INT24), Int24)
      , ((#const MYSQL_TYPE_LONG), Long)
      , ((#const MYSQL_TYPE_FLOAT), Float)
      , ((#const MYSQL_TYPE_DOUBLE), Double)
      , ((#const MYSQL_TYPE_NULL), Null)
      , ((#const MYSQL_TYPE_TIMESTAMP), Timestamp)
      , ((#const MYSQL_TYPE_LONGLONG), LongLong)
      , ((#const MYSQL_TYPE_DATE), Date)
      , ((#const MYSQL_TYPE_TIME), Time)
      , ((#const MYSQL_TYPE_DATETIME), DateTime)
      , ((#const MYSQL_TYPE_YEAR), Year)
      , ((#const MYSQL_TYPE_NEWDATE), NewDate)
      , ((#const MYSQL_TYPE_VARCHAR), VarChar)
      , ((#const MYSQL_TYPE_BIT), Bit)
      , ((#const MYSQL_TYPE_NEWDECIMAL), NewDecimal)
      , ((#const MYSQL_TYPE_ENUM), Enum)
      , ((#const MYSQL_TYPE_SET), Set)
      , ((#const MYSQL_TYPE_TINY_BLOB), TinyBlob)
      , ((#const MYSQL_TYPE_MEDIUM_BLOB), MediumBlob)
      , ((#const MYSQL_TYPE_LONG_BLOB), LongBlob)
      , ((#const MYSQL_TYPE_BLOB), Blob)
      , ((#const MYSQL_TYPE_VAR_STRING), VarString)
      , ((#const MYSQL_TYPE_STRING), String)
      , ((#const MYSQL_TYPE_GEOMETRY), Geometry)
      , (mysql_type_json, Json)
      ]

data MYSQL_BIND = MYSQL_BIND
    { mysqlBindBufferType :: Type
    , mysqlBindBuffer :: Ptr ()
    , mysqlBindBufferLength :: CULong
    , mysqlBindLength :: Ptr CULong
    , mysqlBindIsNull :: Ptr MyBool
    , mysqlBindIsUnsigned :: MyBool
    , mysqlBindError :: Ptr MyBool
    }

instance Storable MYSQL_BIND where
    sizeOf _    = #{size MYSQL_BIND}
    alignment _ = #{alignment MYSQL_BIND} 
    peek ptr =
        MYSQL_BIND
            <$> (toType <$> (#peek MYSQL_BIND, buffer_type) ptr)
            <*> (#peek MYSQL_BIND, buffer) ptr
            <*> (#peek MYSQL_BIND, buffer_length) ptr
            <*> (#peek MYSQL_BIND, length) ptr
            <*> (#peek MYSQL_BIND, is_null) ptr
            <*> (#peek MYSQL_BIND, is_unsigned) ptr
            <*> (#peek MYSQL_BIND, error) ptr
    poke ptr val = do
       (#poke MYSQL_BIND, buffer_type) ptr $ fromType $ mysqlBindBufferType val
       (#poke MYSQL_BIND, buffer) ptr $ mysqlBindBuffer val
       (#poke MYSQL_BIND, buffer_length) ptr $ mysqlBindBufferLength val
       (#poke MYSQL_BIND, length) ptr $ mysqlBindLength val
       (#poke MYSQL_BIND, is_null) ptr $ mysqlBindIsNull val 
       (#poke MYSQL_BIND, is_unsigned) ptr $ mysqlBindIsUnsigned val 
       (#poke MYSQL_BIND, error) ptr $ mysqlBindError val 

data MYSQL_TIME = MYSQL_TIME
    { mysqlTimeYear :: CUInt 
    , mysqlTimeMonth :: CUInt 
    , mysqlTimeDay :: CUInt 
    , mysqlTimeHour :: CUInt 
    , mysqlTimeMinute :: CUInt 
    , mysqlTimeSecond :: CUInt 
    , mysqlTimeNeg :: Bool
    , mysqlTimeSecondPart :: CULong 
    }

instance Storable MYSQL_TIME where
    sizeOf _    = #{size MYSQL_TIME}
    alignment _ = #{alignment MYSQL_TIME} 
    peek ptr =
        MYSQL_TIME
            <$> (#peek MYSQL_TIME, year) ptr
            <*> (#peek MYSQL_TIME, month) ptr
            <*> (#peek MYSQL_TIME, day) ptr
            <*> (#peek MYSQL_TIME, hour) ptr
            <*> (#peek MYSQL_TIME, minute) ptr
            <*> (#peek MYSQL_TIME, second) ptr
            <*> (Foreign.toBool <$> ((#peek MYSQL_TIME, neg) ptr :: IO MyBool))
            <*> (#peek MYSQL_TIME, second_part) ptr
    poke ptr val = do
       (#poke MYSQL_TIME, year) ptr $ mysqlTimeYear val
       (#poke MYSQL_TIME, month) ptr $ mysqlTimeMonth val
       (#poke MYSQL_TIME, day) ptr $ mysqlTimeDay val
       (#poke MYSQL_TIME, hour) ptr $ mysqlTimeHour val
       (#poke MYSQL_TIME, minute) ptr $ mysqlTimeMinute val 
       (#poke MYSQL_TIME, second) ptr $ mysqlTimeSecond val 
       (#poke MYSQL_TIME, neg) ptr $ ((Foreign.fromBool $ mysqlTimeNeg val) :: MyBool)
       (#poke MYSQL_TIME, second_part) ptr $ mysqlTimeSecondPart val 

-- | A description of a field (column) of a table.
data Field = Field {
      fieldName :: ByteString   -- ^ Name of column.
    , fieldOrigName :: ByteString -- ^ Original column name, if an alias.
    , fieldTable :: ByteString -- ^ Table of column, if column was a field.
    , fieldOrigTable :: ByteString -- ^ Original table name, if table was an alias.
    , fieldDB :: ByteString        -- ^ Database for table.
    , fieldCatalog :: ByteString   -- ^ Catalog for table.
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

instance Semigroup FieldFlags where
    (<>) (FieldFlags a) (FieldFlags b) = FieldFlags (a .|. b)
    {-# INLINE (<>) #-}

instance Monoid FieldFlags where
    mempty = FieldFlags 0
    {-# INLINE mempty #-}
    mappend = (<>)
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
    alignment _ = #{alignment MYSQL_FIELD}
    peek = peekField
    poke _ _ = return ()   -- Unused, but define it to avoid a warning

type Seconds = Word

data Protocol = TCP
              | Socket
              | Pipe
              | Memory
                deriving (Eq, Read, Show, Enum, Typeable)

data Option =
            -- Options accepted by mysq_options.
              ConnectTimeout Seconds
            | Compress
            | NamedPipe
            | InitCommand ByteString
            | ReadDefaultFile FilePath
            | ReadDefaultGroup ByteString
            | CharsetDir FilePath
            | CharsetName String
            | LocalInFile Bool
            | Protocol Protocol
            | SharedMemoryBaseName ByteString
            | ReadTimeout Seconds
            | WriteTimeout Seconds
#if !defined(MARIADB_BASE_VERSION) && MYSQL_VERSION_ID >= 80000
#else
            | UseRemoteConnection
            | UseEmbeddedConnection
            | GuessConnection
            | ClientIP ByteString
#endif
            | SecureAuth Bool
            | ReportDataTruncation Bool
            | Reconnect Bool
#if !defined(MARIADB_BASE_VERSION) && MYSQL_VERSION_ID >= 80000
#else
            | SSLVerifyServerCert Bool
#endif
            -- Flags accepted by mysql_real_connect.
            | FoundRows
            | IgnoreSIGPIPE
            | IgnoreSpace
            | Interactive
            | LocalFiles
            | MultiResults
            | MultiStatements
            | NoSchema
              deriving (Eq, Read, Show, Typeable)

toConnectFlag :: Option -> CULong
toConnectFlag Compress  = #const CLIENT_COMPRESS
toConnectFlag FoundRows = #const CLIENT_FOUND_ROWS
toConnectFlag IgnoreSIGPIPE = #const CLIENT_IGNORE_SIGPIPE
toConnectFlag IgnoreSpace = #const CLIENT_IGNORE_SPACE
toConnectFlag Interactive = #const CLIENT_INTERACTIVE
toConnectFlag LocalFiles = #const CLIENT_LOCAL_FILES
toConnectFlag MultiResults = #const CLIENT_MULTI_RESULTS
toConnectFlag MultiStatements = #const CLIENT_MULTI_STATEMENTS
toConnectFlag NoSchema = #const CLIENT_NO_SCHEMA
toConnectFlag _        = 0
