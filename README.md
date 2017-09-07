ua-parser Erlang Library for the [uap-core](https://github.com/ua-parser/uap-core) project.

## Related Links

 * [uap-core](https://github.com/ua-parser/uap-core)
 * [ua-parser Specification](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md)

## TODO

 * validate against test data
 * handle input of binary UA
 * application
 * cache

# Preflight

Get a copy of [`regexs.yaml`](https://github.com/ua-parser/uap-core/blob/master/regexes.yaml) with:

    make regexs.yaml

# Usage

    application:start(yamerl),
    f(),
    UAP = uap_erlang:load({file,"/usr/src/uap-erlang/regexes.yaml"}),
    UA = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0",
    uap_erlang:parse(UA, UAP).

# API

## `uap_erlang` library

### `load({file | string, list()}) -> uap()`

Loads in YAML in the [expected format](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md) from either a file/path or in-memory string.

### `parse(UA, UAP) -> [uap_ua(), uap_os(), uap_device()]`

Same as `parse(UA, UAP, [ua, os, device])`.

### `parse(UA, uap(), Order) -> [uap_ua() | uap_os() | uap_device()]`

Parses the User-Agent in passed in as the string `UA`.
