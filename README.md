ua-parser Erlang Library for the [uap-core](https://github.com/ua-parser/uap-core) project.

## Related Links

 * [uap-core](https://github.com/ua-parser/uap-core)
 * [ua-parser Specification](https://github.com/ua-parser/uap-core/blob/master/docs/specification.md)

# Preflight

Get a copy of [`regexs.yaml`](https://github.com/ua-parser/uap-core/blob/master/regexes.yaml) with:

    make regexs.yaml

# Usage

    application:start(yamerl),
    f(),
    UA = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0",
    UAP = uap_erlang:load({file,"/usr/src/uap-erlang/regexes.yaml"}),
    uap_erlang:parse(UA, UAP).

# TODO

 * validate against test data
 * handle input of binary UA
 * application
 * cache
