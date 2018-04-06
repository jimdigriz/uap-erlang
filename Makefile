PROJECT = uap
PROJECT_DESCRIPTION = ua-parser Erlang Library for the uap-core project
PROJECT_VERSION = 0.1.0

DEPS = yamerl

CT_SUITES = $(patsubst test/%_SUITE.erl,%,$(wildcard test/*_SUITE.erl))

CT_DATA_DIRS = $(foreach c,$(CT_SUITES),test/$(c)_SUITE_data)

include erlang.mk

print-%:
	@echo '$*=$($*)'

priv $(CT_DATA_DIRS):
	mkdir -p $@

priv/regexes.yaml: priv
	curl -f --compressed -L -o $@ https://raw.githubusercontent.com/ua-parser/uap-core/master/regexes.yaml

.PHONY: testdata
testdata: $(foreach d,$(CT_DATA_DIRS),$(d)/regexes.yaml) $(foreach t,ua os device,test/uap_SUITE_data/$(t).yaml)

test/%_SUITE_data/regexes.yaml: priv/regexes.yaml test/%_SUITE_data
	ln -f -s ../../$< $@

test/uap_SUITE_data/%.yaml:
	curl -f --compressed -L -o $@ https://raw.githubusercontent.com/ua-parser/uap-core/master/tests/test_$(notdir $@)
