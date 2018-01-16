{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module:      Database.MySQL.Base.C
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
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
import Data.Semigroup (Semigroup, (<>))
import Data.Typeable (Typeable)
import Data.Word (Word, Word8)
import Foreign.C.Types (CChar, CInt, CUInt, CULong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), peekByteOff)
import qualified Data.IntMap as IntMap

data MYSQL
data MYSQL_RES
data MYSQL_ROWS
type MYSQL_ROW = Ptr (Ptr CChar)
type MYSQL_ROW_OFFSET = Ptr MYSQL_ROWS
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
               ((#const MYSQL_TYPE_INT24), Int24),
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
            | UseRemoteConnection
            | UseEmbeddedConnection
            | GuessConnection
            | ClientIP ByteString
            | SecureAuth Bool
            | ReportDataTruncation Bool
            | Reconnect Bool
            | SSLVerifyServerCert Bool
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
