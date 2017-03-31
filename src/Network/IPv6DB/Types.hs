{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.IPv6DB.Types where

import           Data.Aeson      as A
import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Data.Text.Encoding
import qualified Data.Vector     as V
import           Prelude         hiding (error)
import           Text.IPv6Addr

newtype Address = Address IPv6Addr deriving (Eq, Show)

instance ToJSON Address where
  toJSON (Address (IPv6Addr a)) = String a

instance FromJSON Address where
  parseJSON (String s) =
    case maybeIPv6Addr s of
      Just a  -> pure (Address a)
      Nothing -> fail "Not An IPv6 Address"
  parseJSON _          = fail "JSON String Expected"

data Entries = Entries [Entry]

instance FromJSON Entries where
  parseJSON (Array v) = do
    let ents = fromJSON <$> V.toList v
    if all isSuccess ents
      then pure (Entries $ fromResult <$> ents)
      else fail "Malformed JSON Array"
  parseJSON _           = fail "JSON Array Expected"

data Entry =
  Entry
    { list    :: !T.Text
    , address :: !Address
    } deriving (Eq, Show)

instance FromJSON Entry where
  parseJSON (Object o) = do
    list    <- o .: "list"
    address <- o .: "address"
    pure Entry{..}
  parseJSON _          = fail "JSON Object Expected"

data RedisResponse
  = RedisOk
  | RedisError
      { entry :: !Entry
      , error :: !BS.ByteString
      }
   deriving (Eq, Show)

instance ToJSON RedisResponse where
  toJSON (RedisError{ entry=Entry{..}, .. }) =
    object 
      [ "list"    .= list
      , "address" .= address
      , "error"   .= decodeUtf8 error
      ]
  toJSON _ = Null

data RedisErrors = RedisErrors [RedisResponse] deriving (Eq, Show)

instance ToJSON RedisErrors where
  toJSON (RedisErrors rrs) =
    object [ ("errors", Array (V.fromList $ toJSON <$> rrs)) ]

data Addresses = Addresses [Address]

instance FromJSON Addresses where
  parseJSON (Array v) = do
    let rslts = fromJSON <$> V.toList v
    if all isSuccess rslts
      then pure (Addresses $ fromResult <$> rslts)
      else fail "Bad JSON Array Of IPv6 Addresses"
  parseJSON _         = fail "JSON Array Expected"

data Source = Source !Value deriving (Eq, Show)

instance ToJSON Source where
  toJSON (Source v) = v

instance FromJSON Source where
  parseJSON v = pure (Source v)

data Resource
  = Resource
      { list    :: !T.Text
      , address :: !Address
      , ttl     :: !(Maybe Integer)
      , source  :: !Source
      }
  | ResourceError
      { list    :: !T.Text
      , address :: !Address
      , error   :: !T.Text
      } 
  deriving (Eq, Show)

instance ToJSON Resource where
  toJSON Resource{..} =
    object
      [ "list"    .= list
      , "address" .= address
      , "ttl"     .= ttl
      , "source"  .= source
      ]
  toJSON ResourceError{..} =
    object
      [ "list"    .= list
      , "address" .= address
      , "error"   .= error
      ]

instance FromJSON Resource where
  parseJSON =
    withObject "resource" $
      \o -> do
        list    <- o .: "list"
        address <- do
          ma <- o .: "address"
          case maybeIPv6Addr ma of
            Just a  -> pure (Address a)
            Nothing -> fail "Not an IPv6 Address"
        ttl     <- o .: "ttl"
        source  <- o .: "source"
        return Resource{..}

data Resources = Resources [Resource] deriving (Eq, Show)

instance ToJSON Resources where
  toJSON (Resources rs) =
    object [ ("resources", Array (V.fromList $ toJSON <$> rs)) ]

instance FromJSON Resources where
  parseJSON (Array v) = do
    let rsrcs = fromJSON <$> V.toList v
    if all isSuccess rsrcs
      then pure $ Resources (toRsrc <$> rsrcs)
      else fail "Malformed JSON Array Of Resources"
  parseJSON _           = fail "JSON Array Expected"

isSuccess :: Result a -> Bool
isSuccess (A.Success _) = True
isSuccess (A.Error _)   = False

toRsrc :: Result Resource -> Resource
toRsrc (A.Success r) = r
toRsrc (A.Error _)   = undefined

fromResult :: Result a -> a
fromResult (A.Success e) = e
fromResult (A.Error _)   = undefined

