{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad            (zipWithM)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader
import           Data.Aeson               as A
import           Data.ByteString          hiding (all, notElem, zipWith)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe
import           Data.Text.Encoding
import qualified Data.Vector              as V
import qualified Database.Redis           as R
import           Network.HTTP.Types       hiding (noContent204)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative      as O hiding (empty)
import           Prelude                  hiding (error)
import           Text.IPv6Addr

import           Network.IP6WS
import           Network.IP6WS.Types

data Options = Options { optPort :: Port }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "RESTful Web Service for IPv6 related data"
  <> header "ip6ws v1, (c) Michel Boucey 2017" )

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
  -v --version
  -redis-database
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
  run optPort ip6ws

ip6ws :: Application
ip6ws req res = do
  conn <- R.checkedConnect R.defaultConnectInfo
  withEnv Env { redisConn = conn } $

    case parseMethod (requestMethod req) of
      Right mtd ->
        case pathInfo req of

          ["ip6ws","v1","batch"] ->
            batchHandler mtd

          ["ip6ws","v1","list",list,"addresses"] ->
            listHandler mtd list

          ["ip6ws","v1","list",list,"addresses",addr] ->
            listAddressHandler mtd list addr

          _ -> liftIO $ jsonError "Bad URI Request"

      Left _ -> liftIO $ jsonError "Bad HTTP Method"

    where

      getRBreq :: FromJSON a => IO (Maybe a)
      getRBreq = A.decode <$> strictRequestBody req

      -- -----------------------------------------------------------------------
      -- URI Handlers                                                         --
      -- -----------------------------------------------------------------------

      batchHandler mtd = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            mrsrcs <- getRBreq
            case mrsrcs of
              Just (Resources rsrcs) -> do
                --TODO!
                mapM_ (setSource redisConn mtd' . Just) rsrcs
                noContent204
  {-
                errs <-mapM (setSource conn mtd') rsrcs
                case errs of
                  [] -> noContent204
                  _ -> jsonError "Send array of errors"
  -}
              Nothing -> badJSONRequest

          GET -> do
            ments <- getRBreq
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
            ments <- getRBreq
            case ments of
              Just ents -> do
                ed <- R.runRedis redisConn (delByEntries ents)
                case ed of
                  Right d ->
                    case d of
                      0 -> jsonNotFound "No Deletion Performed"
                      _ -> noContent204
                  Left _  -> jsonError "Error"
              Nothing -> badJSONRequest

          _      -> methodNotAllowed

      listHandler mtd list = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            rb <- getRBreq
            case rb of
              Just (Array vv) -> do
                let rsrcs = (\obj -> maybeResource obj [("list",String list)]) <$> V.toList vv
                if Nothing `notElem` rsrcs
                  then do
                    merrs <- mapM (setSource redisConn mtd') rsrcs
                    if all (== Nothing) merrs
                      then noContent204
                      else jsonError "Compile errors to json"-- TODO errors treatment here
                  else badJSONRequest
              _               -> badJSONRequest

          GET -> do
            maddrs <- getRBreq
            case maddrs of
              Just addrs -> do
                emrs <- R.runRedis redisConn (getByAddresses list addrs)
                case emrs of
                  Right mrs ->
                    withEnv env (fromAddresses list addrs mrs) >>= jsonOk
                  Left  _   -> jsonServerError "Backend Failure"
              Nothing -> badJSONRequest

          DELETE -> do
            maddrs <- getRBreq
            case maddrs of
              Just addrs -> do
                ed <- R.runRedis redisConn (delByAddresses list addrs)
                case ed of
                  Right d ->
                    case d of
                      0 -> jsonNotFound "Resource To Delete Not Found"
                      _ -> noContent204
                  Left _  -> jsonServerError "Backend Failure"
              Nothing -> badJSONRequest

          _      -> methodNotAllowed

      listAddressHandler mtd list addr = do
        Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST -> do
            mjsono <- getRBreq
            case mjsono of
              Just jsono ->
                case maybeResource jsono [("list",String list),("address",String addr)] of
                  jrsrc@(Just _) -> do
                    ssr <- setSource redisConn mtd' jrsrc
                    case ssr of
                      Just RedisOk           -> noContent204
                      Just (re@RedisError{}) -> jsonRes400 (redisErrorToJson re)
                      Nothing                -> jsonError "UNDEFINED"
                  Nothing -> jsonError "Bad JSON Request"
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
                          Nothing   -> jsonError ""
                      Nothing  -> jsonNotFound "Resource Not Found"
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
                        res $
                          jsonRes status404 $
                            resourceError list addr' "The Resource Doesn't Exist"
                  Left _ -> jsonError "Error"
              Nothing -> jsonError "Not an IPv6 Address in URI"

          _      -> methodNotAllowed

      -- -----------------------------------------------------------------------
      -- JSON Responses                                                       --
      -- -----------------------------------------------------------------------

      jsonOk bs = res (jsonRes status200 bs)

      noContent204 = res (responseLBS status204 [] BSL.empty)

      jsonRes400 bs = res (jsonRes status400 bs)

      badJSONRequest = jsonError "Bad JSON Request"

      jsonError err = res (jsonRes status400 $ errorObject err)

      jsonNotFound msg = res (jsonRes status404 $ errorObject msg)

      jsonServerError err = res (jsonRes status500 $ errorObject err)

      errorObject err = "{\"error\":\"" <> err <> "\"}"

      methodNotAllowed =
        res (jsonRes status405 $ errorObject "Method Not Allowed")

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
          toJson Entry{..} Nothing = noResource list address

      fromAddresses list (Addresses addrs) msrcs = do
        bsl <- zipWithM toJson addrs msrcs
        return (toJSONResources bsl)
        where
          toJson addr (Just src) = do
            Env{..} <- ask
            liftIO (justResource redisConn list addr src)
          toJson addr Nothing = noResource list addr

      toJSONResources bsl =
        BSL.fromStrict $ "{\"resources\":[" <> intercalate "," bsl <> "]}"

      justResource conn list (Address (IPv6Addr addr)) src = do
        mttl <- ttlSource conn list addr
        return
          (BSL.toStrict $ encode $ fromJust $ toResource list addr mttl src)

      -- TODO address may be not valid
      noResource list (Address (IPv6Addr addr)) =
        return
          (  "{\"list\":\""
          <> encodeUtf8 list
          <> "\",\"address\":\""
          <> encodeUtf8 addr
          -- TODO
          <> "\",\"error\":\"Resource Not Found\"}" )

      redisErrorToJson RedisError{ entry=Entry{..}, .. } =
        resourceError list (fromAddress address) error
      redisErrorToJson RedisOk = undefined

      resourceError list addr err = BSL.fromStrict $
        "{\"list\":\"" <> encodeUtf8 list <> "\",\"address\":\""
        <> encodeUtf8 addr <> "\",\"error\":\"" <> err <> "\"}"

