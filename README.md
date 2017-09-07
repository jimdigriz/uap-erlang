ua-parser Erlang Library for the [uap-core](https://github.com/ua-parser/uap-core) project.

## Related Links

 * [uap-core](https://github.com/ua-parser/uap-core)
 * [ua-parser Specification](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md)

## TODO

 * fix/investigate why the device tests fail (UA and OS are fine though)

# Preflight

Get a copy of [`regexs.yaml`](https://github.com/ua-parser/uap-core/blob/master/regexes.yaml) with:

    make regexs.yaml

# Usage

## Library

From a `make all shell` you should be able to just run:

    application:start(yamerl),
    rr(uap),
    f(),
    {ok, UAP} = uap:state({file,"regexes.yaml"}),
    UA = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0",
    uap:parse(UA, UAP).
    
    [#uap_ua{family = "Firefox",major = "55",minor = "0",
             patch = undefined},
     #uap_os{family = "Linux",major = undefined,minor = undefined,
             patch = undefined,patch_minor = undefined},
     #uap_device{family = "Other",brand = undefined,
                 model = undefined}]

The record is populated by the same format as the input type of your User-Agent, either a string or binary.

## Standalone Server

    {ok,_} = uap_server:start([{priv,myapp}]).
    uap_server:parse(UA, [os]).

Supported configuration variables are:

 * **`priv` (default: `uap`):** application name for the `priv` directory where `regexes.yaml` is located
 * **`file` (default: `regexes.yaml`):** name of the regexes file to load
 * **`cache` (default: 1000):** number of entries for the lookup cache (can also be `0` for disabled and `unlimited`)

## Application

Add to your `rel/sys.config` so that when the application (re)starts it knows how to build its internal state:

    {uap, [{priv,APP}]}

Now just add to the `applications` section of your `APP.app.src` file `uap`.

### `erlang.mk`

Add to your `Makefile`:

    DEPS += uap
    dep_uap = git https://gitlab.com/jimdigriz/uap-erlang.git master

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
