{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- ^ 'Basic' type definitions for types that are needed by other types, but cannot appear in the same module due to usage of TemplateHaskell
module Moneydb.Types.Basic.Types (Record(..), entityToRecord, TypeName(..), Interval(..), Availability(..), AccountKind(..), prefixModifier, prefixModifier', prefixModifier'') where

import           Control.Lens         ((&), (.~), (?~))
import           Control.Monad        (mzero)
import           Data.Aeson           (FromJSON (..), ToJSON (..), ToJSONKey, Value (..), object, pairs, (.:), (.=))
import qualified Data.Aeson.Encoding  as E
import           Data.Char            (isLower, toLower)
import           Data.Decimal         (Decimal, DecimalRaw (..), roundTo)
import           Data.Int             (Int64)
import           Data.Proxy           (Proxy (..))
import           Data.Scientific      (normalize, scientific)
import           Data.Swagger
import qualified Data.Text            as T
import           Database.Persist     (Entity (..), Key, PersistField (..), PersistValue (..), ToBackendKey)
import           Database.Persist.Sql (PersistFieldSql (..), SqlBackend, SqlType (..), fromSqlKey)
import           Database.Persist.TH
import           GHC.Generics         (Datatype, Generic, M1, Rep, datatypeName, from)

data Record a = Record {recordId :: Int64, recordData :: a} deriving Show

entityToRecord :: ToBackendKey SqlBackend a => Entity a -> Record a
entityToRecord (Entity x y) = Record (fromSqlKey x) y

instance ToJSON a => ToJSON (Record a) where
    -- this generates a Value
    toJSON c =
        object ["id" .= recordId c, "data" .= recordData c]

    -- this encodes directly to a bytestring Builder
    toEncoding c =
        pairs ("id" .= recordId c <> "data" .= recordData c)

instance FromJSON a => FromJSON (Record a) where
   parseJSON (Object v) = Record <$>
                          v .: "id" <*>
                          v .: "data"
   parseJSON _          = mzero

-- | A class of types from which we can get the name as a string.
--
-- This class can be automatically derived, e.g. with
-- `deriving instance TypeName MyType`.
class TypeName a where
  typeName :: Proxy a -> String

  default typeName :: (Generic a, GTypeName (Rep a)) => Proxy a -> String
  typeName _proxy = gtypeName (from (undefined :: a))

-- | Generic equivalent to `TypeName`.
class GTypeName f where
  gtypeName :: f a -> String

instance (Datatype c) => GTypeName (M1 i c f) where
  gtypeName = datatypeName

instance (ToSchema a, TypeName a) => ToSchema (Record a) where
  declareNamedSchema _ = do
    intSchema <- declareSchemaRef (Proxy :: Proxy Int64)
    dataSchema <- declareSchemaRef (Proxy :: Proxy a)
    -- let s = toSchema (Proxy :: Proxy a)
    -- return . NamedSchema (Just $ "Record" <> fromJust (_schemaTitle s)) $ mempty
    return . NamedSchema (Just $ "Record" <> T.pack (typeName (Proxy :: Proxy a))) $ mempty
    -- return . NamedSchema Nothing $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
           [("id", intSchema), ("data", dataSchema)]
      & required .~ ["id", "data"]

instance ToSchema (DecimalRaw Integer) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Double)

instance ToSchema (Key a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToJSON Decimal where
    toJSON (Decimal e m) = Number . scientific m . (* (-1)) . fromIntegral $ e
    toEncoding = E.scientific . fromRational . toRational

instance FromJSON Decimal where
    parseJSON (Number s) = pure . fromRational . toRational . normalize $ s
    parseJSON v          = fail $ "Expected Scientific, got " ++ show v

instance PersistField Decimal where
   toPersistValue = PersistInt64 . fromIntegral . decimalMantissa . roundTo 2
   fromPersistValue (PersistInt64 c) = Right . Decimal 2 . fromIntegral $ c
   fromPersistValue x = Left . T.pack $ "fromPersistValue (Decimal) :: Expected PersistInt64, got: " ++ show x

instance PersistFieldSql Decimal where
  sqlType _ = SqlInt32

data Interval = Daily | Weekly | Monthly | Yearly deriving (Read, Show, Eq, FromJSON, ToJSON, Generic, ToSchema)
derivePersistField "Interval"

data Availability = Immediately | Weeks | Months | Years | Decades deriving (Read, Show, Eq, Ord, FromJSON, ToJSON, Generic, ToSchema, Enum, Bounded, ToJSONKey)
derivePersistField "Availability"

data AccountKind = Cash | Credit | Debit | Debt | Virtual | Investment | Prepayment
    deriving (Read, Show, Eq, FromJSON, ToJSON, Generic, ToSchema)
derivePersistField "AccountKind"

fstToLower :: String -> String
fstToLower (x:xs) = toLower x : xs
fstToLower []     = []

-- handy for ToJSON / fromJSON and ToSchema
prefixModifier :: String -> String
prefixModifier = fstToLower . dropWhile isLower

-- in case of complicated type names
prefixModifier' :: TypeName a => Proxy a -> String -> String
prefixModifier' x = fstToLower . drop (length $ typeName x)

-- in case of complicated type names
prefixModifier'' :: String -> String -> String
prefixModifier'' x = fstToLower . drop (length x)
