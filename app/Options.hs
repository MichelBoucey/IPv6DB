
module Options where

import           Data.ByteString
import           Data.Monoid         ((<>))
import           Options.Applicative

data Options =
  Options
    { appPort       :: Int
    , redisHost     :: String
    , redisPort     :: Integer
    , redisDatabase :: Integer
    , redisAuth     :: Maybe ByteString
    , logFile       :: String
    }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
    <> progDesc "RESTful Web Service for IPv6 related data"
    <> header "IPv6DB v0.1.0 APIv1, (c) Michel Boucey 2017" )

options :: Parser Options
options =
  Options
    <$>
      option auto
        ( short 'p'
          <> long "port"
          <> help "Alternative listening port"
          <> showDefault
          <> value 4446
          <> metavar "" )
    <*>
      strOption
        ( short 'h'
          <> long "redis-host"
          <> help "Redis host"
          <> showDefault
          <> value "localhost" )
    <*>
      option auto
        ( short 'r'
          <> long "redis-port"
          <> help "Redis listening port"
          <> showDefault
          <> value 6379
          <> metavar "" )
    <*>
      option auto
        ( short 'd'
          <> long "redis-database"
          <> help "Redis database"
          <> showDefault
          <> value 0 )
    <*>
      option auto
        ( short 'a'
          <> long "redis-auth"
          <> help "Redis authentication password"
          <> value Nothing )
    <*>
      strOption
        ( short 'l'
          <> long "log-file"
          <> help "Log file"
          <> showDefault
          <> value "/var/log/ipv6db.log" )

