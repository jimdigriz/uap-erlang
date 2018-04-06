-module(uap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("src/uap.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([groups/0, init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2]).

-export([list/1]).
-export([binary/1]).

-export([ua/2, os/2, device/2]).	% INTERNAL

-define(TYPES, [ua,os,device]).

all() ->
	[
		{group,ua},
		{group,os},
		{group,device}
	].

groups() ->
	[
		{types,[parallel],[list,binary]},
		{ua,[parallel],[{group,types}]},
		{os,[parallel],[{group,types}]},
		{device,[parallel],[{group,types}]}
	].

init_per_suite(Config) ->
	application:ensure_all_started(yamerl),
	DataDir = ?config(data_dir, Config),
	{ok, UAP} = uap:state(file, filename:join([DataDir, "regexes.yaml"])),
	[{uap,UAP}|Config].

end_per_suite(_Config) ->
	ok.

init_per_group(types, Config) ->
	Config;
init_per_group(Test, Config) ->
	DataDir = ?config(data_dir, Config),
	[YAML] = yamerl_constr:file(filename:join([DataDir, atom_to_list(Test) ++ ".yaml"])),
	Tests = proplists:get_value("test_cases", YAML),
	[{test,Test},{tests,Tests}|Config].

end_per_group(_, _Config) ->
	ok.

init_per_testcase(Type, Config) when Type == list ->
	[{type,Type}|Config];
init_per_testcase(Type, Config0) when Type == binary ->
	Tests0 = ?config(tests, Config0),
	Config = proplists:delete(tests, Config0),
	Tests = lists:map(fun(Test) ->
		lists:map(fun({K,V}) ->
			{K,to_binary(V)}
		end, Test)
	end, Tests0),
	[{tests,Tests},{type,Type}|Config].

list(Config) ->
	Type = ?config(type, Config),
	Test = ?config(test, Config),
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "~s ~s (~b tests)", [Test, Type, length(Tests)]),
	true = lists:all(fun(X) -> ?MODULE:Test(X, UAP) end, Tests).
binary(Config) ->
	list(Config).

%%

to_binary(X) when is_list(X) ->
	unicode:characters_to_binary(X);
to_binary(X) ->
	X.

-define(EXPECTED(X), null2undefined(proplists:get_value(??X, Test))).
-define(PASS(X), if X =/= Expected -> ct:log(error, ?HI_IMPORTANCE, "'~s' failed: ~p (expected: ~p)", [UA, X, Expected]), false; true -> true end).

null2undefined(null) -> undefined;
null2undefined(X) -> X.

ua(Test, UAP) ->
	UA = proplists:get_value("user_agent_string", Test),
	[UAP_UA] = uap:parse(UA, [ua], UAP),
	Expected = #uap_ua{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch)
	},
	?PASS(UAP_UA).

os(Test, UAP) ->
	UA = proplists:get_value("user_agent_string", Test),
	[UAP_OS] = uap:parse(UA, [os], UAP),
	Expected = #uap_os{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch),
		patch_minor	= ?EXPECTED(patch_minor)
	},
	?PASS(UAP_OS).

device(Test, UAP) ->
	UA = proplists:get_value("user_agent_string", Test),
	[UAP_Device] = uap:parse(UA, [device], UAP),
	Expected = #uap_device{
		family		= ?EXPECTED(family),
		brand		= ?EXPECTED(brand),
		model		= ?EXPECTED(model)
	},
	?PASS(UAP_Device).
