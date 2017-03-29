
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
        ( long "redis-host"
          <> help "Redis host"
          <> showDefault
          <> value "localhost" )
    <*>
      option auto
        ( short 'p'
          <> long "port"
          <> help "Redis listening port"
          <> showDefault
          <> value 6379
          <> metavar "" )
    <*>
      option auto
        ( long "redis-database"
          <> help "Redis database"
          <> showDefault
          <> value 0 )
    <*>
      option auto
        ( long "redis-auth"
          <> help "Redis authentication password"
          <> value Nothing )

