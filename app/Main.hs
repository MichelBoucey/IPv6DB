{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad            (zipWithM)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader
import           Data.Aeson               as A
import           Data.ByteString          hiding (all, filter, notElem, zipWith)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import           Data.Text.Encoding
import qualified Data.Vector              as V
import qualified Database.Redis           as R
import           Network.HTTP.Types       hiding (noContent204)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative      as O hiding (empty)
import           Prelude                  hiding (error)
import           Text.IPv6Addr

import           Network.IPv6DB
import           Network.IPv6DB.Types

data Options = Options { optPort :: Port }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "RESTful Web Service for IPv6 related data"
  <> header "IPv6DB v1, (c) Michel Boucey 2017" )

options :: O.Parser Options
options = Options <$> port

port :: O.Parser Int
port = O.option auto
  ( short 'p'
  <> long "port"
  <> help "Alternative listening port"
  <> showDefault
  <> value 4446
  <> metavar "" )

{- TODO
 - command line options
  --version -v
  --redis-database
  --redis-port
  --redis-host
  --redis-auth
-}

data Env = Env { redisConn :: R.Connection }

withEnv :: forall r (m :: * -> *) a. r
        -> ReaderT r m a
        -> m a
withEnv = flip runReaderT

main :: IO ()
main = do
  Options{..} <- execParser opts
  run optPort ipv6db

ipv6db :: Application
ipv6db req res = do
  conn <- R.checkedConnect R.defaultConnectInfo
  withEnv Env { redisConn = conn } $

    case parseMethod (requestMethod req) of
      Right mtd ->
        case pathInfo req of

          ["ipv6db","v1","batch"] ->
            batchHandler mtd

          ["ipv6db","v1","list",list,"addresses"] ->
            listHandler mtd list

          ["ipv6db","v1","list",list,"addresses",addr] ->
            listAddressHandler mtd list addr

          _ -> liftIO $ jsonError "Bad URI Request"

      Left _ -> liftIO $ jsonError "Bad HTTP Method"

    where

      getRequestBody :: FromJSON a => IO (Maybe a)
      getRequestBody = A.decode <$> strictRequestBody req

      -- -----------------------------------------------------------------------
      -- URI Handlers                                                         --
      -- -----------------------------------------------------------------------

      batchHandler mtd = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            mrsrcs <- getRequestBody
            case mrsrcs of
              Just (Resources rsrcs) -> do
                results <- mapM (setSource redisConn mtd') rsrcs
                if all (== RedisOk) results
                  then noContent204
                  else jsonRes400 (fromRedisErrors results)
              Nothing -> badJSONRequest

          GET -> do
            ments <- getRequestBody
            case ments of
              Just ents -> do
                msrcs <- R.runRedis redisConn (getByEntries ents)
                case msrcs of
                  Right srcs -> do
                    bs <- withEnv env (fromEntries ents srcs)
                    jsonOk bs
                  Left  _   -> jsonError "Error"
              Nothing -> badJSONRequest

          DELETE -> do
            ments <- getRequestBody
            case ments of
              Just ents -> do
                ed <- R.runRedis redisConn (delByEntries ents)
                case ed of
                  Right d ->
                    case d of
                      0 -> jsonRes404 (justError "No Deletion Performed")
                      _ -> noContent204
                  Left _  -> jsonError "Error"
              Nothing -> badJSONRequest

          _      -> methodNotAllowed

      listHandler mtd list = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            rb <- getRequestBody
            case rb of
              Just (Array v) -> do
                let rsrcs =
                      (\o -> maybeResource o [("list",String list)]) <$> V.toList v
                if Nothing `notElem` rsrcs
                  then do
                    results <- mapM (setSource redisConn mtd'. fromJust) rsrcs
                    if all (== RedisOk) results
                      then noContent204
                      else jsonRes400 (fromRedisErrors results)
                  else badJSONRequest
              _              -> badJSONRequest

          GET -> do
            maddrs <- getRequestBody
            case maddrs of
              Just addrs -> do
                emsrcs <- R.runRedis redisConn (getByAddresses list addrs)
                case emsrcs of
                  Right msrcs ->
                    withEnv env (fromAddresses list addrs msrcs) >>= jsonOk
                  Left  _     -> jsonServerError "Backend Failure"
              Nothing -> badJSONRequest

          DELETE -> do
            maddrs <- getRequestBody
            case maddrs of
              Just addrs -> do
                ed <- R.runRedis redisConn (delByAddresses list addrs)
                case ed of
                  Right d ->
                    case d of
                      0 -> jsonRes404 (justError "Resource To Delete Not Found")
                      _ -> noContent204
                  Left _  -> jsonServerError "Backend Failure"
              Nothing -> badJSONRequest

          _      -> methodNotAllowed

      listAddressHandler mtd list addr = do
        Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            mo <- getRequestBody
            case mo of
              Just o ->
                case maybeResource o [("list",String list),("address",String addr)] of
                  Nothing -> jsonRes400 (justError "Bad JSON Request")
                  Just rsrc   -> do
                    rdres <- setSource redisConn mtd' rsrc
                    case rdres of
                      RedisOk -> noContent204
                      error   -> jsonRes400 (redisErrorToJson error)
              Nothing -> badJSONRequest

          GET ->
            case maybeIPv6Addr addr of
              Just (IPv6Addr addr') -> do
                emsrc <- liftIO (R.runRedis redisConn $ getSource list addr')
                case emsrc of
                  Right msrc ->
                    case msrc of
                      Just src -> do
                        ttls <- ttlSource redisConn list addr'
                        case toResource list addr' ttls src of
                          Just rsrc -> jsonOk (A.encode rsrc)
                          Nothing   -> jsonError ""-- TODO!
                      Nothing  ->
                        jsonRes404 (resourceError list addr' "Resource Not Found")
                  Left _     -> jsonError "Error"
              Nothing -> jsonError "Not IPv6 Address in URI"

          DELETE ->
            case maybeIPv6Addr addr of
              Just (IPv6Addr addr') -> do
                er <- liftIO (R.runRedis redisConn $ delSource list addr')
                case er of
                  Right i ->
                    case i of
                      1 -> noContent204
                      _ ->
                        jsonRes404 $
                          resourceError list addr' "The Resource Doesn't Already Exist"
                  Left _ -> jsonError "Error"--TODO!
              Nothing -> jsonError "Not an IPv6 Address in URI"

          _      -> methodNotAllowed

      -- -----------------------------------------------------------------------
      -- JSON Responses                                                       --
      -- -----------------------------------------------------------------------

      jsonOk bs = res (jsonRes status200 bs)

      noContent204 = res (responseLBS status204 [] BSL.empty)

      jsonRes400 bs = res (jsonRes status400 bs)

      jsonRes404 bs = res (jsonRes status404 bs)

      badJSONRequest = jsonError "Bad JSON Request"

      jsonError err = res (jsonRes status400 $ justError err)

      jsonServerError err = res (jsonRes status500 $ justError err)

      justError err = "{\"error\":\"" <> err <> "\"}"

      methodNotAllowed =
        res (jsonRes status405 $ justError "Method Not Allowed")

      jsonRes status =
        responseLBS
          status
          [ ("Content-Type", "application/json; charset=utf-8") ]

      fromEntries (Entries ents) msrcs = do
        bsl <- zipWithM toJson ents msrcs
        return (toJSONResources bsl)
        where
          toJson Entry{..} (Just src) = do
            Env{..} <- ask
            liftIO (justResource redisConn list address src)
          toJson Entry{..} Nothing =
            return $
              BSL.toStrict $
                resourceError list (fromAddress address) "Resource Not Found"

      fromAddresses list (Addresses addrs) msrcs = do
        bsl <- zipWithM toJson addrs msrcs
        return (toJSONResources bsl)
        where
          toJson addr (Just src) = do
            Env{..} <- ask
            liftIO (justResource redisConn list addr src)
          toJson (Address (IPv6Addr addr)) Nothing =
            return $
              BSL.toStrict (resourceError list addr "Resource Not Found")

      fromRedisErrors errs =
        "{\"errors\":[" <>
        BSL.intercalate "," (filter (/= BSL.empty) $ redisErrorToJson <$> errs)
        <> "]}"

      toJSONResources bsl =
        BSL.fromStrict $ "{\"resources\":[" <> intercalate "," bsl <> "]}"

      justResource conn list (Address (IPv6Addr addr)) src = do
        mttl <- ttlSource conn list addr
        return $
          BSL.toStrict (encode $ fromJust $ toResource list addr mttl src)

      redisErrorToJson RedisError{ entry=Entry{..}, .. } =
        resourceError list (fromAddress address) error
      redisErrorToJson RedisOk = BSL.empty

      resourceError list addr err = BSL.fromStrict $
        "{\"list\":\"" <> encodeUtf8 list <> "\",\"address\":\""
        <> encodeUtf8 addr <> "\",\"error\":\"" <> err <> "\"}"

