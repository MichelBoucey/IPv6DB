{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types where

import           Data.Aeson           as A
import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Vector          as V
import           Prelude              hiding (error)

import           Network.IPv6DB.Types

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

data Entries = Entries [Entry]

instance FromJSON Entries where
  parseJSON (Array v) = do
    let ents = fromJSON <$> V.toList v
    if all isSuccess ents
      then pure (Entries $ fromResult <$> ents)
      else fail "Malformed JSON Array"
  parseJSON _           = fail "JSON Array Expected"

data RedisResponse
  = RedisOk
  | RedisError
      { entry :: !Entry
      , error :: !BS.ByteString
      }
   deriving (Eq, Show)

instance ToJSON RedisResponse where
  toJSON RedisError{ entry=Entry{..}, .. } =
    object
      [ "list"    .= list
      , "address" .= address
      , "error"   .= decodeUtf8 error
      ]
  toJSON _ = Null

data RedisErrors = RedisErrors [RedisResponse] deriving (Eq, Show)

instance ToJSON RedisErrors where
  toJSON (RedisErrors rrs) =
    object
      [ ("errors", Array $ V.fromList $ toJSON <$> filter (/= RedisOk) rrs) ]

