PROJECT = uap
PROJECT_DESCRIPTION = ua-parser Erlang Library for the uap-core project
PROJECT_VERSION = 0.0.1

DEPS = yamerl

CT_SUITES = $(PROJECT)

include erlang.mk

regexes.yaml:
	curl -f --compressed -L -o $@ https://raw.githubusercontent.com/ua-parser/uap-core/master/regexes.yaml

.PHONY: testdata
testdata: test/uap_SUITE_data testdata_yaml

test/uap_SUITE_data:
	mkdir -p $@

.PHONY: testdata_yaml
testdata_yaml: test/uap_SUITE_data/regexes.yaml test/uap_SUITE_data/ua.yaml test/uap_SUITE_data/os.yaml test/uap_SUITE_data/device.yaml

test/uap_SUITE_data/regexes.yaml: regexes.yaml
	ln -f -s ../../regexes.yaml $@

test/uap_SUITE_data/%.yaml:
	curl -f --compressed -L -o $@ https://raw.githubusercontent.com/ua-parser/uap-core/master/tests/test_$(notdir $@)
