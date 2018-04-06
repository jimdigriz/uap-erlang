-module(uap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("src/uap.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2]).
-export([ua/1]).
-export([os/1]).
-export([device/1]).

-define(TYPES, [ua,os,device]).

all() ->
	[{group,list},{group,binary}].

groups() ->
	[{list,[parallel],?TYPES},{binary,[parallel],?TYPES}].

init_per_group(Type, Config) ->
	application:ensure_all_started(yamerl),
	DataDir = ?config(data_dir, Config),
	{ok, UAP} = uap:state(file, filename:join([DataDir, "regexes.yaml"])),
	[{uap,UAP},{type,Type}|Config].

end_per_group(_, _Config) ->
	ok.

init_per_testcase(Test, Config) ->
	DataDir = ?config(data_dir, Config),
	[YAML] = yamerl_constr:file(filename:join([DataDir, atom_to_list(Test) ++ ".yaml"])),
	Tests = proplists:get_value("test_cases", YAML),
	[{tests,Tests}|Config].

ua(Config) ->
	Type = ?config(type, Config),
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "ua ~s (~b tests)", [Type, length(Tests)]),
	true = lists:all(fun(X) -> ua2(X, Type, UAP) end, Tests).

os(Config) ->
	Type = ?config(type, Config),
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "os ~s (~b tests)", [Type, length(Tests)]),
	true = lists:all(fun(X) -> os2(X, Type, UAP) end, Tests).

device(Config) ->
	Type = ?config(type, Config),
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "device ~s (~b tests)", [Type, length(Tests)]),
	true = lists:all(fun(X) -> device2(X, Type, UAP) end, Tests).

%%

-define(EXPECTED(X), null2undefined(type(proplists:get_value(??X, Test), Type))).
-define(PASS(X), if X =/= Expected -> ct:log(error, ?HI_IMPORTANCE, "'~s' failed: ~p (expected: ~p)", [UA, X, Expected]), false; true -> true end).

type(X, binary) when is_list(X) ->
	unicode:characters_to_binary(X);
type(X, _Type) ->
	X.

null2undefined(null) -> undefined;
null2undefined(X) -> X.

ua2(Test, Type, UAP) ->
	UA0 = proplists:get_value("user_agent_string", Test),
	UA = if Type == binary -> unicode:characters_to_binary(UA0); true -> UA0 end,
	[UAP_UA] = uap:parse(UA, [ua], UAP),
	Expected = #uap_ua{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch)
	},
	?PASS(UAP_UA).

os2(Test, Type, UAP) ->
	UA0 = proplists:get_value("user_agent_string", Test),
	UA = if Type == binary -> unicode:characters_to_binary(UA0); true -> UA0 end,
	[UAP_OS] = uap:parse(UA, [os], UAP),
	Expected = #uap_os{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch),
		patch_minor	= ?EXPECTED(patch_minor)
	},
	?PASS(UAP_OS).

device2(Test, Type, UAP) ->
	UA0 = proplists:get_value("user_agent_string", Test),
	UA = if Type == binary -> unicode:characters_to_binary(UA0); true -> UA0 end,
	[UAP_Device] = uap:parse(UA, [device], UAP),
	Expected = #uap_device{
		family		= ?EXPECTED(family),
		brand		= ?EXPECTED(brand),
		model		= ?EXPECTED(model)
	},
	?PASS(UAP_Device).
