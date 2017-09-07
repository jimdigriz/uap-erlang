ua-parser Erlang Library for the [uap-core](https://github.com/ua-parser/uap-core) project.

## Related Links

 * [uap-core](https://github.com/ua-parser/uap-core)
 * [ua-parser Specification](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md)

## TODO

 * fix/investigate why the device tests fail (UA and OS are fine though)
 * application
 * cache

# Preflight

Get a copy of [`regexs.yaml`](https://github.com/ua-parser/uap-core/blob/master/regexes.yaml) with:

    make regexs.yaml

# Usage

From a `make all shell` you should be able to just run:

    application:start(yamerl),
    rr(uap),
    f(),
    UAP = uap:state({file,"regexes.yaml"}),
    UA = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0",
    uap:parse(UA, UAP).
    
    [#uap_ua{family = "Firefox",major = "55",minor = "0",
             patch = undefined},
     #uap_os{family = "Linux",major = undefined,minor = undefined,
             patch = undefined,patch_minor = undefined},
     #uap_device{family = "Other",brand = undefined,
                 model = undefined}]

# API

## `uap` library

### `state({file | string, list()}) -> uap()`

Loads in YAML in the [expected format](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md) from either a provided filepath or in-memory string.

### `parse(UA, UAP) -> [uap_ua(), uap_os(), uap_device()]`

Same as `parse(UA, UAP, [ua, os, device])`.

### `parse(UA, uap(), Order) -> [uap_ua() | uap_os() | uap_device()]`

Parses the User-Agent in passed in as the string `UA`.

# Testing

Fetch a copy of the [test data](https://github.com/ua-parser/uap-core/blob/master/tests/):

    make testdata

Now run the tests with:

    make tests
