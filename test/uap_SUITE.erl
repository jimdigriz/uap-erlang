-module(uap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("src/uap.hrl").

-export([all/0, init_per_testcase/2]).
-export([ua/1]).

all() ->
	[ua].

init_per_testcase(X, Config) ->
	application:start(yamerl),
	DataDir = ?config(data_dir, Config),
	UAP = uap:load({file,DataDir ++ "/regexes.yaml"}),
	[YAML] = yamerl_constr:file(DataDir ++ atom_to_list(X) ++ ".yaml"),
	Tests = proplists:get_value("test_cases", YAML),
	[{uap,UAP},{tests,Tests}|Config].

ua(Config) ->
	UAP = ?config(uap, Config),
	Tests = ?config(tests, Config),
	ct:log(info, ?STD_IMPORTANCE, "ua (~p tests)", [length(Tests)]),
	true = lists:all(fun(X) -> ua2(UAP, X) end, Tests).

ua2(UAP, Test) ->
	UA = proplists:get_value("user_agent_string", Test),
	ct:log(info, ?STD_IMPORTANCE, "~p", [UA]),
	[UAP_UA] = uap:parse(UA, UAP, [ua]),
	Expected = #uap_ua{
		family	= null2undefined(proplists:get_value("family", Test)),
		major	= null2undefined(proplists:get_value("major", Test)),
		minor	= null2undefined(proplists:get_value("minor", Test)),
		patch	= null2undefined(proplists:get_value("patch", Test))
	},
	case UAP_UA == Expected of
		true ->
			true;
		false ->
			ct:log(error, ?HI_IMPORTANCE, "expected: ~p, got: ~p", [Expected, UAP_UA]),
			false
	end.

null2undefined(null) -> undefined;
null2undefined(X) -> X.
