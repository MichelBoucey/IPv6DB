# IPv6DB

## A RESTful Web service for IPv6 related data [![Build Status](https://travis-ci.org/MichelBoucey/IPv6DB.svg?branch=master)](https://travis-ci.org/MichelBoucey/IPv6DB)

IPv6DB is a RESTful microservice using Redis as backend to store lists of IPv6 addresses and attach to each of them data in a schema-free JSON value. Each resource can be permanent or TTLed.

```bash
IPv6DB v0.1.0 APIv1, (c) Michel Boucey 2017

Usage: ipv6db [-p|--port] [-h|--redis-host ARG] [-r|--redis-port]
              [-d|--redis-database ARG] [-a|--redis-auth ARG]
              [-l|--log-file ARG]
  RESTful Web Service for IPv6 related data

Available options:
  -p,--port                Alternative listening port (default: 4446)
  -h,--redis-host ARG      Redis host (default: "localhost")
  -r,--redis-port          Redis listening port (default: 6379)
  -d,--redis-database ARG  Redis database (default: 0)
  -a,--redis-auth ARG      Redis authentication password
  -l,--log-file ARG        Log file (default: "/var/log/ipv6db.log")
  -h,--help                Show this help text
```

A resource example:

```json
    {
      "list": "black",
      "address": "abcd::1234",
      "ttl": 34582,
      "source":
        {
          "services": [25,587,143]
        }
    }
```

See [IPv6DB APIv1](https://github.com/MichelBoucey/IPv6DB/blob/master/IPv6DB_APIv1.md).

The package includes binary and library.
