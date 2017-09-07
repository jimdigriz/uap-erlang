PROJECT = uap
PROJECT_DESCRIPTION = ua-parser Erlang Library for the uap-core project
PROJECT_VERSION = 0.0.1

DEPS = yamerl

include erlang.mk

.PHONY: regexes.yaml
regexes.yaml:
	curl -f --compressed -L -o $@ https://raw.githubusercontent.com/ua-parser/uap-core/master/regexes.yaml

.PHONY: testdata
testdata:
	$(foreach T,ua os device,curl -f --compressed -L -o $(T).yaml https://raw.githubusercontent.com/ua-parser/uap-core/master/tests/test_$(T).yaml;)
