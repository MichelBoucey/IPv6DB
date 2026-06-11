{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Queries where

import           Control.Monad             (zipWithM)
import           Control.Monad.Reader
import qualified Data.Aeson                as A
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.HashMap.Lazy         (fromList)
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import           Data.Text.Encoding
#if MIN_VERSION_hedis(0,16,0)
import qualified Data.List.NonEmpty        as N
import           Database.Redis
import           Database.Redis.Commands   ()
import           Database.Redis.Connection ()
import           Database.Redis.Types      ()
#else
import           Database.Redis
#endif
import           Network.HTTP.Types        (StdMethod (..))
import           Network.IPv6DB.Types
import           Text.IPv6Addr

import           Types

fromEntries :: (MonadReader Env f, MonadIO f)
            => Entries
            -> [Maybe BS.ByteString]
            -> f BSL.ByteString
fromEntries (Entries ents) msrcs =
  A.encode <$> zipWithM toJson ents msrcs
  where
    toJson Entry{..} (Just src) = do
      Env{..} <- ask
      liftIO (buildResource redisConn list address src)
    toJson Entry{..} Nothing =
      return (ResourceError list address "Resource Not Found")

fromAddresses :: (MonadReader Env f, MonadIO f)
              => T.Text
              -> Addresses
              -> [Maybe BS.ByteString]
              -> f BSL.ByteString
fromAddresses list (Addresses addrs) msrcs =
  A.encode <$> zipWithM toJson addrs msrcs
  where
    toJson addr (Just src) = do
      Env{..} <- ask
      liftIO (buildResource redisConn list addr src)
    toJson addr Nothing =
      return (ResourceError list addr "Resource Not Found")

buildResource :: Connection
              -> T.Text
              -> IPv6Addr
              -> BS.ByteString
              -> IO Resource
buildResource conn list (IPv6Addr addr) src = do
  mttl <- ttlSource conn list addr
  return (fromJust $ toResource list addr mttl src)

setSource :: Connection -> StdMethod -> Resource -> IO RedisResponse
setSource _ _ ResourceError{} = undefined
setSource conn mtd Resource{ttl=ttlr,..} = do
  er <- runRedis conn $ setOpts
          (toKey list $ unIPv6Addr address)
          (BSL.toStrict $ A.encode source)
#if MIN_VERSION_hedis(0,16,0)
          SetOpts
            { setSeconds   = ttlr
            , setMilliseconds = Nothing
            , setCondition =
                case mtd of
                  PUT  -> Just Xx
                  POST -> Just Nx
                  _    -> Nothing
            , setUnixSeconds = Nothing
            , setUnixMilliseconds = Nothing
            , setKeepTTL = False
            }
#else
          SetOpts
            { setSeconds   = ttlr
            , setMilliseconds = Nothing
            , setCondition =
                case mtd of
                  PUT  -> Just Xx
                  POST -> Just Nx
                  _    -> Nothing
            }
#endif
  return $
    case er of
      Right s ->
        case s of
          Ok            -> RedisOk
          Status status -> toRedisError list address status
          Pong          -> toRedisError list address "Ping!"
      Left r ->
        case r of
          Error err    -> toRedisError list address err
          Bulk Nothing ->
            case mtd of
              PUT  ->
                toRedisError
                  list
                  address
                  "The Resource Doesn't Exist Yet (Use POST To Create It)"
              POST ->
                toRedisError
                  list
                  address
                  "The Resource Already Exists (Use PUT To Replace It)"
              _    ->
                toRedisError
                  list
                  address
                  "HTTP Method Not Handled"
          Bulk (Just bs) -> toRedisError list address bs
          _                ->
            toRedisError
                list
                address
                "Undefined Redis Error"

toRedisError :: T.Text
             -> IPv6Addr
             -> BS.ByteString
             -> RedisResponse
toRedisError list addr err =
  RedisError
    { entry = toEntry list addr
    , error = err
    }

ttlSource :: Connection
          -> T.Text
          -> T.Text
          -> IO (Maybe Integer)
ttlSource conn list addr = do
  ettl <- runRedis conn (Database.Redis.ttl $ toKey list addr)
  return $
    case ettl of
      Right i ->
        if i > 0
          then Just i
          else Nothing
      Left _  -> Nothing

getSource :: RedisCtx m f
          => T.Text
          -> T.Text
          -> m (f (Maybe BS.ByteString))
getSource list addr = get (toKey list addr)

delSource :: RedisCtx m f
          => T.Text
          -> T.Text
          -> m (f Integer)
delSource list addr =
#if MIN_VERSION_hedis(0,16,0)
  del (N.fromList [ toKey list addr ])
#else
  del [ toKey list addr ]
#endif

toResource :: T.Text
           -> T.Text
           -> Maybe Integer
           -> BS.ByteString
           -> Maybe Resource
toResource list addr mi bs =
  A.decode (BSL.fromStrict bs) >>= \src ->
    Just
      Resource
        { list    = list
        , address = IPv6Addr addr
        , ttl     = mi
        , source  = Source src
        }

maybeResource :: A.Value
              -> [(T.Text,A.Value)]
              -> Maybe Resource
maybeResource v prs =
  case v of
    A.Object hm -> do
      let hm' =
            if KM.member "ttl" hm
              then hm
              else KM.insert "ttl" A.Null hm
      case A.fromJSON (A.Object $ KM.union hm' $ KM.fromHashMapText $ fromList prs) of
        A.Success r -> Just r
        A.Error _   -> Nothing
    _         -> Nothing

getByAddresses :: RedisCtx m f
               => T.Text
               -> Addresses
               -> m (f [Maybe BS.ByteString])
getByAddresses list addrs =
#if MIN_VERSION_hedis(0,16,0)
  mget (N.fromList $ addressesToKeys list addrs)
#else
  mget (addressesToKeys list addrs)
#endif

getByEntries :: RedisCtx m f
             => Entries
             -> m (f [Maybe BS.ByteString])
getByEntries ents =
#if MIN_VERSION_hedis(0,16,0)
  mget (N.fromList $ fromEnts ents)
#else
  mget (fromEnts ents)
#endif

delByAddresses :: RedisCtx m f
               => T.Text
               -> Addresses -> m (f Integer)
delByAddresses list addrs =
#if MIN_VERSION_hedis(0,16,0)
  del (N.fromList $ addressesToKeys list addrs)
#else
  del (addressesToKeys list addrs)
#endif

delByEntries :: RedisCtx m f
             => Entries
             -> m (f Integer)
delByEntries ents =
#if MIN_VERSION_hedis(0,16,0)
  del (N.fromList $ fromEnts ents)
#else
  del (fromEnts ents)
#endif

addressesToKeys :: T.Text
                -> Addresses
                -> [BS.ByteString]
addressesToKeys list (Addresses addrs) =
  toKey list . unIPv6Addr <$> addrs

fromEnts :: Entries -> [BS.ByteString]
fromEnts (Entries ents) =
  (\Entry{..} -> toKey list (unIPv6Addr address)) <$> ents

toEntry :: T.Text -> IPv6Addr -> Entry
toEntry list address = Entry { list = list, address = address }

toKey :: T.Text -> T.Text -> BS.ByteString
toKey list addr =
  encodeUtf8 (list <> listAddressSeparator <> addr)

listAddressSeparator :: T.Text
listAddressSeparator = "/"

