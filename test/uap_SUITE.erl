-module(uap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("src/uap.hrl").

-export([all/0, init_per_testcase/2]).
-export([ua/1]).
-export([os/1]).
-export([device/1]).

all() ->
	[ua,os,device].

init_per_testcase(X, Config) ->
	application:start(yamerl),
	DataDir = ?config(data_dir, Config),
	{ok, UAP} = uap:state({file,filename:join([DataDir, "regexes.yaml"])}),
	[YAML] = yamerl_constr:file(filename:join([DataDir, atom_to_list(X) ++ ".yaml"])),
	Tests = proplists:get_value("test_cases", YAML),
	[{uap,UAP},{tests,Tests}|Config].

ua(Config) ->
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "ua (~p tests)", [length(Tests)]),
	true = lists:all(fun(X) -> ua2(UAP, X) end, Tests).

os(Config) ->
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "os (~p tests)", [length(Tests)]),
	true = lists:all(fun(X) -> os2(UAP, X) end, Tests).

device(Config) ->
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "device (~p tests)", [length(Tests)]),
	true = lists:all(fun(X) -> device2(UAP, X) end, Tests).

%%

-define(EXPECTED(X), null2undefined(proplists:get_value(??X, Test))).
-define(PASS(X), Pass = X == Expected, case Pass of true -> ct:log(info, ?STD_IMPORTANCE, "success: ~p", [X]); false -> ct:log(error, ?HI_IMPORTANCE, "failed: ~p (expected: ~p)", [X, Expected]) end, Pass).

null2undefined(null) -> undefined;
null2undefined(X) -> X.

ua2(UAP, Test) ->
	UA = proplists:get_value("user_agent_string", Test),
	ct:log(info, ?STD_IMPORTANCE, "~p", [UA]),
	[UAP_UA] = uap:parse(UA, UAP, [ua]),
	Expected = #uap_ua{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch)
	},
	?PASS(UAP_UA).

os2(UAP, Test) ->
	UA = proplists:get_value("user_agent_string", Test),
	ct:log(info, ?STD_IMPORTANCE, "~p", [UA]),
	[UAP_OS] = uap:parse(UA, UAP, [os]),
	Expected = #uap_os{
		family		= ?EXPECTED(family),
		major		= ?EXPECTED(major),
		minor		= ?EXPECTED(minor),
		patch		= ?EXPECTED(patch),
		patch_minor	= ?EXPECTED(patch_minor)
	},
	?PASS(UAP_OS).

device2(UAP, Test) ->
	UA = proplists:get_value("user_agent_string", Test),
	ct:log(info, ?STD_IMPORTANCE, "~p", [UA]),
	[UAP_Device] = uap:parse(UA, UAP, [device]),
	Expected = #uap_device{
		family		= ?EXPECTED(family),
		brand		= ?EXPECTED(brand),
		model		= ?EXPECTED(model)
	},
	?PASS(UAP_Device).
