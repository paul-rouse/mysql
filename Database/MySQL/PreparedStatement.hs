{-# LANGUAGE ScopedTypeVariables #-}
module Database.MySQL.PreparedStatement
    where

import           Control.Monad             (void, when)
import           Data.ByteString.Char8
import           Data.ByteString.Internal  (create)
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import qualified Data.Fixed                as Fixed
import           Data.Text                 (Text)
import qualified Data.Text.Foreign         as Text
import           Data.Time.Calendar        (Day, fromGregorian, toGregorian)
import           Data.Time.LocalTime       (LocalTime (..), TimeOfDay (..),
                                            makeTimeOfDayValid)
import           Database.MySQL.Base.C
import           Database.MySQL.Base.Types hiding (Type (..))
import qualified Database.MySQL.Base.Types as Types
import           Foreign                   hiding (newForeignPtr, void)
import           Foreign.C.Error
import           Foreign.C.String          (peekCString)
import           Foreign.Concurrent        (newForeignPtr)

data Statement = Statement
    { connectionPtr :: ForeignPtr MYSQL
    , statementPtr  :: ForeignPtr MYSQL_STMT
    }

prepare :: ForeignPtr MYSQL -> Text -> IO Statement
prepare mysql q =
    withForeignPtr mysql $ \mysql' -> do
    mysqlStmt <- throwIfNull "mysql_stmt_init" $ mysql_stmt_init mysql'
    stmt <- newForeignPtr mysqlStmt (void $ mysql_stmt_close mysqlStmt)
    Text.withCStringLen q $ \(p,l) -> do
        res <- mysql_stmt_prepare mysqlStmt p (fromIntegral l)
        when (res > 0) $ do
            err <- peekCString =<< mysql_stmt_error mysqlStmt
            error err
    pure $ Statement mysql stmt

withConn :: Statement -> (Ptr MYSQL -> IO a) -> IO a
withConn statement =
    withForeignPtr (connectionPtr statement)

withStatement :: Statement -> (Ptr MYSQL_STMT -> IO a) -> IO a
withStatement statement =
    withForeignPtr (statementPtr statement)

data Value
    = Null
    | TinyInt Int8
    | Short Int16
    | MediumInt Int32
    | Long Int32
    | LongLong Int64
    | UnsignedTinyInt Word8
    | UnsignedShort Word16
    | UnsignedMediumInt Word32
    | UnsignedLong Word32
    | UnsignedLongLong Word64
    | Float Float
    | Double Double
    | Decimal ByteString
    | Time TimeOfDay
    | Date Day
    | DateTime LocalTime
    | Timestamp LocalTime
    | String Text
    | Blob ByteString
    deriving (Eq, Show)

withValue :: Value -> (MYSQL_BIND -> IO a) -> IO a
withValue v k =
    case v of
        Null ->
            k defaultBind
        TinyInt i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Tiny
                    , mysqlBindBuffer = castPtr ptr
                    }
        Short i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Short
                    , mysqlBindBuffer = castPtr ptr
                    }

        MediumInt i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Long
                    , mysqlBindBuffer = castPtr ptr
                    }

        Long i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Long
                    , mysqlBindBuffer = castPtr ptr
                    }
        LongLong i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.LongLong
                    , mysqlBindBuffer = castPtr ptr
                    }
        UnsignedTinyInt i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Tiny
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindIsUnsigned = 1
                    }
        UnsignedShort i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Short
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindIsUnsigned = 1
                    }

        UnsignedMediumInt i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Long
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindIsUnsigned = 1
                    }

        UnsignedLong i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Long
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindIsUnsigned = 1
                    }
        UnsignedLongLong i ->
            with i $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.LongLong
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindIsUnsigned = 1
                    }
        Float f ->
            with f $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Float
                    , mysqlBindBuffer = castPtr ptr
                    }
        Double d ->
            with d $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Double
                    , mysqlBindBuffer = castPtr ptr
                    }

        Decimal _ ->
            error "Unsupported input type: Decimal"

        String t ->
            Text.withCStringLen t $ \(ptr, l) ->
            with (fromIntegral l) $ \lPtr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.String
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindBufferLength = fromIntegral l
                    , mysqlBindLength = lPtr
                    }

        Blob b ->
            unsafeUseAsCStringLen b $ \(ptr, l) ->
            with (fromIntegral l) $ \lPtr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Blob
                    , mysqlBindBuffer = castPtr ptr
                    , mysqlBindBufferLength = fromIntegral l
                    , mysqlBindLength = lPtr
                    }

        Date day ->
            let (year, month, dayOfMonth) = toGregorian day
                myTime = MYSQL_TIME
                    { mysqlTimeYear = fromIntegral year
                    , mysqlTimeMonth = fromIntegral month
                    , mysqlTimeDay = fromIntegral dayOfMonth
                    , mysqlTimeHour = 0
                    , mysqlTimeMinute = 0
                    , mysqlTimeSecond = 0
                    , mysqlTimeNeg = False
                    , mysqlTimeSecondPart = 0
                    }
            in
            with myTime $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Date
                    , mysqlBindBuffer = castPtr ptr
                    }

        Time t ->
            let (seconds, secondParts) = picoToSecondsAndMicroseconds (todSec t)
                myTime = MYSQL_TIME
                    { mysqlTimeYear = 0
                    , mysqlTimeMonth = 0
                    , mysqlTimeDay = 0
                    , mysqlTimeHour = fromIntegral $ todHour t
                    , mysqlTimeMinute = fromIntegral $ todMin t
                    , mysqlTimeSecond = fromInteger seconds
                    , mysqlTimeNeg = False
                    , mysqlTimeSecondPart = fromInteger secondParts
                    }
            in
            with myTime $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Time
                    , mysqlBindBuffer = castPtr ptr
                    }
        DateTime dt ->
            with (localTime dt) $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.DateTime
                    , mysqlBindBuffer = castPtr ptr
                    }

        Timestamp ts ->
            with (localTime ts) $ \ptr ->
                k $ defaultBind
                    { mysqlBindBufferType = Types.Timestamp
                    , mysqlBindBuffer = castPtr ptr
                    }

  where
      localTime t =
        let (year, month, dayOfMonth) = toGregorian $ localDay t
            tod = localTimeOfDay t
            (seconds, secondParts) = picoToSecondsAndMicroseconds (todSec tod)
        in
        MYSQL_TIME
            { mysqlTimeYear = fromIntegral year
            , mysqlTimeMonth = fromIntegral month
            , mysqlTimeDay = fromIntegral dayOfMonth
            , mysqlTimeHour = fromIntegral $ todHour tod
            , mysqlTimeMinute = fromIntegral $ todMin tod
            , mysqlTimeSecond = fromInteger seconds
            , mysqlTimeNeg = False
            , mysqlTimeSecondPart = fromInteger secondParts
            }

      defaultBind =
         MYSQL_BIND
            { mysqlBindBufferType = Types.Null
            , mysqlBindBuffer = nullPtr
            , mysqlBindBufferLength = 0
            , mysqlBindLength = nullPtr
            , mysqlBindIsNull = nullPtr
            , mysqlBindIsUnsigned = 0
            , mysqlBindError = nullPtr
            }

bindParams :: Statement -> [Value] -> IO ()
bindParams statement args =
    withStatement statement $ \mysqlStmt -> do
    throwErrnoIf_ (> 0) "mysql_stmt_bind_param" $
        withMany withValue args $ \binds ->
        withArray binds $ mysql_stmt_bind_param mysqlStmt

execute :: Statement -> IO ()
execute statement =
    withStatement statement $ \mysqlStmt -> do
        throwErrnoIf_ (> 0)  "mysql_stmt_execute" $ mysql_stmt_execute mysqlStmt

storeResult :: Statement -> IO ()
storeResult statement =
    withStatement statement $ \mysqlStmt -> do
        throwErrnoIf_ (> 0)  "mysql_stmt_store_result" $ mysql_stmt_store_result mysqlStmt

fetchFieldMetadata :: Ptr MYSQL_STMT -> IO [Field]
fetchFieldMetadata mysqlStmt = do
    mysqlRes <- mysql_stmt_result_metadata mysqlStmt
    if mysqlRes == nullPtr then do
        err <- peekCString =<< mysql_stmt_error mysqlStmt
        error err
    else do
        numFields <- mysql_num_fields mysqlRes
        fields <- peekArray (fromIntegral numFields) =<< mysql_fetch_fields mysqlRes
        mysql_free_result mysqlRes
        pure fields

withField :: Field -> (MYSQL_BIND -> IO a) -> IO a
withField field k =
    let len = if fieldMaxLength field /= 0 then fieldMaxLength field else fieldLength field
        isUnsigned = fromBool $ hasAllFlags (flagNumeric <> flagUnsigned) (fieldFlags field)
    in
    alloca $ \isNull ->
    alloca $ \lenPtr ->
    allocaBytes (fromIntegral len) $ \buffer ->
    k $ MYSQL_BIND
        { mysqlBindBufferType = fieldType field
        , mysqlBindBuffer = buffer
        , mysqlBindBufferLength = fromIntegral len
        , mysqlBindLength = lenPtr
        , mysqlBindIsNull = isNull
        , mysqlBindIsUnsigned = isUnsigned
        , mysqlBindError = nullPtr
        }

withBoundResultFields :: Ptr MYSQL_STMT -> [Field] -> ([MYSQL_BIND] -> IO a) -> IO a
withBoundResultFields mysqlStmt fields k =
    withMany withField fields $ \binds ->
    withArray binds $ \bindsPtr -> do
    throwErrnoIf_ (> 0)  "mysql_stmt_bind_result" $ mysql_stmt_bind_result mysqlStmt bindsPtr
    k binds

toValue :: MYSQL_BIND -> IO (Maybe Value)
toValue bind = do
    isNull <- peek $ mysqlBindIsNull bind
    if toBool isNull then
        pure Nothing
    else do
        case mysqlBindBufferType bind of
            Types.Null ->
                pure  Nothing

            Types.Tiny -> do
                if toBool $ mysqlBindIsUnsigned bind then do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ UnsignedTinyInt i
                else do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ TinyInt i

            Types.Short -> do
                if toBool $ mysqlBindIsUnsigned bind then do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ UnsignedShort i
                else do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ Short i

            Types.Int24 -> do
                if toBool $ mysqlBindIsUnsigned bind then do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ UnsignedMediumInt i
                else do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ MediumInt i

            Types.Long -> do
                if toBool $ mysqlBindIsUnsigned bind then do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ UnsignedLong i
                else do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ Long i

            Types.LongLong -> do
                if toBool $ mysqlBindIsUnsigned bind then do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ UnsignedLongLong i
                else do
                    i <- peek $ castPtr $ mysqlBindBuffer bind
                    pure $ Just $ LongLong i

            Types.Float -> do
                f <- peek $ castPtr $ mysqlBindBuffer bind
                pure $ Just $ Float f

            Types.Double -> do
                d <- peek $ castPtr $ mysqlBindBuffer bind
                pure $ Just $ Double d

            Types.NewDecimal -> do
                length' <- peek $ mysqlBindLength bind
                blob <- create (fromIntegral length') $ \d ->
                        copyBytes d (castPtr $ mysqlBindBuffer bind) (fromIntegral length')
                pure $ Just $ Decimal blob

            Types.String -> do
                length' <- peek $ mysqlBindLength bind
                blob <- Text.peekCStringLen (castPtr $ mysqlBindBuffer bind, fromIntegral length')
                pure $ Just $ String blob

            Types.VarString -> do
                length' <- peek $ mysqlBindLength bind
                blob <- Text.peekCStringLen (castPtr $ mysqlBindBuffer bind, fromIntegral length')
                pure $ Just $ String blob

            Types.Date -> do
                day <- peekDay $ castPtr (mysqlBindBuffer bind)
                pure $ Just $ Date day

            Types.Time -> do
                tod <- peekTimeOfDay $ castPtr (mysqlBindBuffer bind)
                pure $ fmap Time tod

            Types.DateTime -> do
                localTime <- peekLocalTime $ castPtr (mysqlBindBuffer bind)
                pure $ fmap DateTime localTime

            Types.Timestamp -> do
                localTime <- peekLocalTime $ castPtr (mysqlBindBuffer bind)
                pure $ fmap Timestamp localTime

            _ -> do
                length' <- peek $ mysqlBindLength bind
                blob <- create (fromIntegral length') $ \d ->
                        copyBytes d (castPtr $ mysqlBindBuffer bind) (fromIntegral length')
                pure $ Just $ Blob blob

peekLocalTime :: Ptr MYSQL_TIME -> IO (Maybe LocalTime)
peekLocalTime =
    fmap mysqlTimeToLocalTime . peek

mysqlTimeToLocalTime :: MYSQL_TIME -> Maybe LocalTime
mysqlTimeToLocalTime myTime =
    LocalTime (mysqlTimeToDay myTime) <$> mysqlTimeToTimeOfDay myTime

peekDay :: Ptr MYSQL_TIME -> IO Day
peekDay =
    fmap mysqlTimeToDay . peek

mysqlTimeToDay :: MYSQL_TIME -> Day
mysqlTimeToDay myTime =
    fromGregorian (fromIntegral $ mysqlTimeYear myTime)
                  (fromIntegral $ mysqlTimeMonth myTime)
                  (fromIntegral $ mysqlTimeDay myTime)

peekTimeOfDay :: Ptr MYSQL_TIME -> IO (Maybe TimeOfDay)
peekTimeOfDay =
    fmap mysqlTimeToTimeOfDay . peek

mysqlTimeToTimeOfDay :: MYSQL_TIME -> Maybe TimeOfDay
mysqlTimeToTimeOfDay myTime =
    let pico = picoFromSecondsAndMicroseconds
                    (fromIntegral $ mysqlTimeSecond myTime)
                    (Fixed.MkFixed $ fromIntegral $ mysqlTimeSecondPart myTime)
    in makeTimeOfDayValid (fromIntegral $ mysqlTimeHour myTime)
                          (fromIntegral $ mysqlTimeMinute myTime)
                          pico

unFixed :: Fixed.Fixed a -> Integer
unFixed (Fixed.MkFixed i) = i

scaleFixed :: forall a b.
              (Fixed.HasResolution a, Fixed.HasResolution b)
           => Fixed.Fixed a -> Fixed.Fixed b
scaleFixed x =
    let inputRes = Fixed.resolution x
        outputRes = Fixed.resolution (undefined :: Fixed.Fixed b)
    in Fixed.MkFixed (unFixed x * outputRes `div` inputRes)

picoToSecondsAndMicroseconds :: Fixed.Pico -> (Integer, Integer)
picoToSecondsAndMicroseconds picos =
    let (seconds, rest) = properFraction picos
        micros = scaleFixed rest :: Fixed.Micro
    in (seconds, unFixed micros)

picoFromSecondsAndMicroseconds :: Integer -> Fixed.Micro -> Fixed.Pico
picoFromSecondsAndMicroseconds seconds micro =
    scaleFixed $ fromInteger seconds + micro

withRow :: Ptr MYSQL_STMT -> [MYSQL_BIND] -> ([Maybe Value] -> IO a) -> IO a
withRow mysqlStmt binds k = do
    res <- mysql_stmt_fetch mysqlStmt
    if res == 0 then do
        vals <- traverse toValue binds
        k vals
    else
        k []

withRows :: Ptr MYSQL_STMT -> [MYSQL_BIND] -> ([[Maybe Value]] -> IO a) -> IO a
withRows mysqlStmt binds k = do
    withRow mysqlStmt binds $ \row ->
        if Prelude.null row then
            k []
        else
            withRows mysqlStmt binds $ \rows ->
                k $ row : rows

fetchResults :: Statement -> IO [[Maybe Value]]
fetchResults statement =
    withStatement statement $ \mysqlStmt -> do
    fields <- fetchFieldMetadata mysqlStmt
    withBoundResultFields mysqlStmt fields $ \binds ->
        withRows mysqlStmt binds pure
