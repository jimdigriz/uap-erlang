Erlang User-Agent Parser Library for the [uap-core](https://github.com/ua-parser/uap-core) project.

## Related Links

 * [uap-core](https://github.com/ua-parser/uap-core)
 * [ua-parser Specification](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md)

# Preflight

Get a copy of [`regexes.yaml`](https://github.com/ua-parser/uap-core/blob/master/regexes.yaml) with:

    make priv/regexes.yaml

# Usage

## Library

From a `make all shell` you should be able to just run:

    application:ensure_all_started(yamerl),
    rr(uap),
    f(),
    {ok, UAP} = uap:state(file, "priv/regexes.yaml"),
    UA = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0",
    uap:parse(UA, UAP).
    
    {ok, [
       #uap_ua{family = "Firefox",major = "55",minor = "0",
             patch = undefined},
       #uap_os{family = "Linux",major = undefined,minor = undefined,
             patch = undefined,patch_minor = undefined},
       #uap_device{family = "Other",brand = undefined,
                 model = undefined}
    ]}

## Standalone Server

    application:ensure_all_started(yamerl),
    rr(uap).
    {ok, _} = uap_server:start_link().
    uap_server:parse(UA, [os]).

## Application

Add to your `rel/sys.config` so that when the application (re)starts it knows how to build its internal state:

    {uap, [{priv,APP}]}

Now just add to the `applications` section of your `APP.app.src` file `uap`.

### `erlang.mk`

Add to your `Makefile`:

    DEPS += uap
    dep_uap = git https://gitlab.com/jimdigriz/uap-erlang.git master

# API

## Library

### `state(file | string, iodata()) -> {ok, uap()}`

Loads in YAML in the [expected format](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md) from either a provided filepath or in-memory string.

Throws on error.

### `parse(iodata(), uap()) -> {ok, [uap_ua() | uap_os() | uap_device()]} | {error,atom()}`

Same as `parse(iodata(), [ua, os, device], uap())`.

### `parse(iodata(), Order, uap()) -> {ok, [uap_ua() | uap_os() | uap_device()]} | {error,atom()}`

Parses the User-Agent in passed in as the string `UA`.

**N.B.** from the unit tests, it seems that parsing binaries is faster than lists

## Server/Application

### `start_link()`

Same as `start_link([])`.

### `start_link(list(proplists:property()))`

Supported configuration properties are:

 * **`priv` (default: `uap`):** application name for the `priv` directory where `regexes.yaml` is located
 * **`file` (default: `regexes.yaml`):** name of the regexes file to load
 * **`cache` (default: 10000):** number of entries for the lookup cache (can also be `0` for disabled and `unlimited`)

### `parse(iodata(), list(ua | os | device))` -> {ok, [uap_ua() | uap_os() | uap_device()]} | {error,atom()}`

Same as `parse(iodata(), list(ua | os | device), [])`.

### `parse(iodata(), list(ua | os | device), list(proplists:property()))` -> {ok, [uap_ua() | uap_os() | uap_device()]} | {error,atom()}`

Supported properties are:

 * **`normalize` (default: false):**
   * improves the effectiveness of the cache
   * recommended only when the cardinality of user-agents is high
   * suitable for use *only* require `#uap_{ua,os}.{family,major}`
   * ignored when `start_link/1` is called with `{cache,0}`

An an error is returned if you duplicate any of the atoms `ua` `os` or `device` in the list of results to return.

# Testing

Fetch a copy of the [test data](https://github.com/ua-parser/uap-core/blob/master/tests/):

    make testdata

Now run the tests (takes about five minutes and needs 4GB of RAM) with:

    make tests

If you just want to test the `uap-core` test data (takes about 30 seconds and needs 4GB of RAM) use:

    make tests CT_SUITES=uap
