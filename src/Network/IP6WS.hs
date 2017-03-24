{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.IP6WS
  ( setSource
  , getSource
  , delSource
  , ttlSource
  , getByAddresses
  , delByAddresses
  , getByEntries
  , delByEntries
  , toResource
  , toRedisError
  , maybeResource
  , fromAddress
  )
  where

import           Data.Aeson           as A
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Database.Redis       as R hiding (decode)
import           Network.HTTP.Types   (StdMethod(..))
import           Text.IPv6Addr

import           Network.IP6WS.Types  as I
import           Data.HashMap.Lazy

setSource ::Connection -> StdMethod -> Maybe Resource -> IO (Maybe RedisError)
setSource _ _ Nothing = pure Nothing
setSource conn mtd (Just Resource{ttl=ttlr,..}) = do
  er <- runRedis conn $ setOpts
          (toKey list $ fromAddress address)
          (BSL.toStrict $ encode source)
          SetOpts
            { setSeconds   = ttlr
            , setMilliseconds = Nothing
            , setCondition =
                case mtd of
                  PUT  -> Just Xx
                  POST -> Just Nx
                  _    -> Nothing
            }
  return $
    case er of
      Right s ->
        case s of
          Ok            -> Just RedisOk
          Status status -> Just (toRedisError list address status)
          Pong          -> Just (toRedisError list address "Ping!")
      Left r ->
        case r of
          R.Error err      -> Just (toRedisError list address err)
          R.Bulk Nothing   ->
            case mtd of
              PUT  -> Just $
                toRedisError
                  list
                  address
                  "The Resource Doesn't Exist Yet (Use POST To Create It)"
              POST -> Just $
                toRedisError
                  list
                  address
                  "The Resource Already Exists (Use PUT To Replace It)"
              _    ->
                Just (toRedisError list address "HTTP Method Not Handled")
          R.Bulk (Just bs) -> Just (toRedisError list address bs)
          _                ->
            Just (toRedisError list address "Redis Type Error Not handled Yet")

toRedisError :: T.Text -> Address -> BS.ByteString -> RedisError
toRedisError list addr err =
  RedisError { entry = toEntry list addr, error = err }

ttlSource :: Connection
          -> T.Text
          -> T.Text
          -> IO (Maybe Integer)
ttlSource conn list addr = do
  ettl <- R.runRedis conn (R.ttl $ toKey list addr)
  return $
    case ettl of
      Right i ->
        if i > 0
          then Just i
          else Nothing
      Left _    -> Nothing

getSource :: RedisCtx m f
          => T.Text
          -> T.Text
          -> m (f (Maybe BS.ByteString))
getSource list addr = get (toKey list addr)

delSource :: RedisCtx m f
          => T.Text
          -> T.Text
          -> m (f Integer)
delSource list addr = del [ toKey list addr ]

toResource :: T.Text
           -> T.Text
           -> Maybe Integer
           -> BS.ByteString
           -> Maybe Resource
toResource list addr mi bs =
  case decode (BSL.fromStrict bs) of
    Just source -> Just
      Resource
        { list    = list
        , address = Address (IPv6Addr addr)
        , ttl     = mi
        , source  = Source source
        }
    Nothing     -> Nothing

getByAddresses :: RedisCtx m f
               => T.Text
               -> Addresses
               -> m (f [Maybe BS.ByteString])
getByAddresses list addrs =
  mget (addressesToKeys list addrs)

getByEntries :: RedisCtx m f
             => Entries
             -> m (f [Maybe BS.ByteString])
getByEntries ents = mget (fromEntries ents)

delByAddresses :: RedisCtx m f
               => T.Text
               -> Addresses -> m (f Integer)
delByAddresses list addrs =
  del (addressesToKeys list addrs)

delByEntries :: RedisCtx m f
             => Entries
             -> m (f Integer)
delByEntries ents = del (fromEntries ents)

addressesToKeys :: T.Text
                -> Addresses
                -> [BS.ByteString]
addressesToKeys list addrs =
  toKey list <$> fromAddresses addrs

fromEntries :: Entries -> [BS.ByteString]
fromEntries (Entries ents) =
  (\Entry{..} -> toKey list (fromAddress address)) <$> ents

fromAddresses :: Addresses -> [T.Text]
fromAddresses (Addresses addrs) =
  fromAddress <$> addrs

fromAddress :: Address -> T.Text
fromAddress (Address (IPv6Addr addr)) = addr

toKey :: T.Text -> T.Text -> BS.ByteString
toKey list addr =
  encodeUtf8 (list <> listAddressSeparator <> addr)

listAddressSeparator :: T.Text
listAddressSeparator = "/"

maybeResource :: Value
              -> [(T.Text,Value)]
              -> Maybe Resource
maybeResource v prs =
  case v of
    Object hm ->
      case fromJSON (Object $ union hm $ fromList prs) of
        A.Success r -> Just r
        A.Error _   -> Nothing
    _         -> Nothing

toEntry :: T.Text -> Address -> Entry
toEntry list address = Entry { list = list, address = address }

