-module(uap_erlang).

-export([load/1]).
-export([parse/2]).

-include("uap.hrl").

-record(uap_re, {
	re	:: tuple(),
	replace	:: list()
}).

-define(UAP_FIELDS_UA, ["family_replacement", "v1_replacement", "v2_replacement", "v3_replacement"]).
-define(UAP_FIELDS_OS, ["os_replacement", "os_v1_replacement", "os_v2_replacement", "os_v3_replacement", "os_v4_replacement"]).
-define(UAP_FIELDS_DEVICE, ["device_replacement", "brand_replacement", "model_replacement"]).

load({Source, Pointer}) when Source == file; Source == string ->
	[YAML] = yamerl_constr:Source(Pointer),
	UAP = lists:map(fun load/1, YAML),
	{?MODULE, UAP};
load({"user_agent_parsers", REs}) ->
	load2(?UAP_FIELDS_UA, REs);
load({"os_parsers", REs}) ->
	load2(?UAP_FIELDS_OS, REs);
load({"device_parsers", REs}) ->
	load2(?UAP_FIELDS_DEVICE, REs).

load2(Fields, REs) ->
	lists:map(fun(PL) -> load3(Fields, PL) end, REs).

load3(Fields, PL) ->
	RE = proplists:get_value("regex", PL),
	{ok, MP} = re:compile(RE),
	Replace = lists:map(fun(Y) -> proplists:get_value(Y, PL) end, Fields),
	#uap_re{ re = MP, replace = Replace }.

parse(UA, {?MODULE, UAP}) when is_list(UA) ->
	Results = lists:map(fun(X) -> parse2(UA, ["Other"], X) end, UAP),
	lists:map(fun parse4/1, lists:zip([ua, os, device], Results)).

parse2(_UA, Default, []) ->
	Default;
parse2(UA, Default, [RE = #uap_re{ re = MP }|REs]) ->
	Match = re:run(UA, MP, [{capture,all_but_first,list}]),
	parse3(UA, Default, REs, RE, Match).

parse3(_UA, _Default, _REs, #uap_re{ replace = Replace }, {match, Captured}) ->
	Replace2 = lists:zip(Replace, lists:seq(1, length(Replace))),
	lists:map(fun(X) -> replace(X, Captured) end, Replace2);
parse3(UA, Default, REs, _RE, nomatch) ->
	parse2(UA, Default, REs).

parse4({ua, L}) ->
	L2 = L ++ lists:duplicate(size(#uap_ua{}) - 1 - length(L), undefined),
	list_to_tuple([uap_ua|L2]);
parse4({os, L}) ->
	L2 = L ++ lists:duplicate(size(#uap_os{}) - 1 - length(L), undefined),
	list_to_tuple([uap_os|L2]);
parse4({device, L}) ->
	L2 = L ++ lists:duplicate(size(#uap_device{}) - 1 - length(L), undefined),
	list_to_tuple([uap_device|L2]).

replace({undefined, N}, Captured) when N > length(Captured) ->
	undefined;
replace({undefined, N}, Captured) ->
	lists:nth(N, Captured);
replace({R, _N}, Captured) ->
	replace2(R, Captured, []).

replace2([], _Captured, RN) ->
	RN;
replace2(["$",X|R], Captured, RN) when X >= $1, X =< $9 ->
	replace2(R, Captured, RN ++ lists:nth(list_to_integer(X), Captured));
replace2([X|R], Captured, RN) ->
	replace2(R, Captured, RN ++ [X]).
