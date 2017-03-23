# ip6ws APIv1

## A RESTful Web service for IPv6-related data

Default Web service port: 4446

### /ip6ws/v1/list/\{*listName*\}/addresses/{*IPv6Address*}

__POST /ip6ws/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

Create an IPv6 address with its related data to the given list.

    POST /ip6ws/v1/list/hosts/addresses/abcd::1234

```json
    {
      "ttl": null,
      "source":
        {
          "services": ["smtp","imap"]
        }
    }
```

__PUT /ip6ws/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

Replace an IPv6 address with its related data to the given list.

```
    PUT /ip6ws/v1/list/hosts/addresses/abcd::1234
```

```json
    {
      "ttl": null,
      "source":
        {
          "services": ["smtp","imap","ssh"]
        }
    }
```

Response:

If successful, returns HTTP Status 204 No Content

__GET /ip6ws/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

```
    GET /ip6ws/v1/list/hosts/addresses/abcd::1234
```

Response:

```json
    {
      "list": "hosts",
      "address": "abcd::1234",
      "ttl": null,
      "source":
        {
          "services": ["smtp","imap","ssh"]
        }
    }
```

__DELETE /ip6ws/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

```
    DELETE /ip6ws/v1/hosts/addresses/abcd::1234
```

Response:

If successful, returns an empty response body.

### /ip6ws/v1/list/\{*listName*\}/addresses

__POST /ip6ws/v1/list/\{__*listName*__\}/addresses__

Create many IPv6 addresses with their related data to the given list.

```json
    POST /ip6ws/v1/hosts/addresses
    [
      {
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap","ssh"]
          }
      },
      {
        "address": "abcd::1235",
        "ttl": null,
        "source":
          {
            "services": ["http","https","ssh"]
          }
      }
    ]
```

__PUT /ip6ws/v1/list/\{__*listName*__\}/addresses__

Replace many IPv6 addresses with their related data to the given list.

```json
    PUT /ip6ws/v1/hosts/addresses
    [
      {
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap","ssh"]
          }
      },
      {
        "address": "abcd::1235",
        "ttl": null,
        "source":
          {
            "services": ["http","https","ssh"]
          }
      }
    ]
```

Replace many IPv6 addresses with their related data to the given list.

__GET /ip6ws/v1/list/\{__*listName*__\}/addresses__

Get data related to the given IPv6address.

```json
    GET /ip6ws/v1/hosts/addresses
    [
      "abcd::1234",
      "abcd::1235"
    ]
```

__DELETE /ip6ws/v1/list/\{__*listName*__\}/addresses__

Delete many IPv6 addresses from the given list.

```json
    DELETE /ip6ws/v1/hosts/addresses
    [
      "abcd::1234",
      "abcd::1235"
    ]
```

### /ip6ws/v1/batch

__PUT /ip6ws/v1/batch__

```json
    PUT /ip6ws/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap"]
          }
      },
      {
        "list": "blacklist",
         "address": "bad0:1235",
         "ttl": 604800,
         "source": null
      }
    ]
```

__GET /ip6ws/v1/batch__

```json
    GET /ip6ws/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234"
      },
      {
        "list": "blacklist",
        "address": "bad0:1234"
      }
    ]
```

__DELETE /ip6ws/v1/batch__

```json
    DELETE /ip6ws/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234"
      },
      {
        "list": "backlist",
        "address": "bad0:1234"
      }
    ]
```

