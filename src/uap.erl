-module(uap).

-export([load/1]).
-export([parse/2, parse/3]).

-include("uap.hrl").

-record(uap, {
	ua	:: uap_re(),
	os	:: uap_re(),
	device	:: uap_re()
}).

-record(uap_re, {
	re	:: tuple(),
	replace	:: list()
}).

-type uap_re() :: #uap_re{}.

-define(UAP_FIELDS_UA, ["family_replacement", "v1_replacement", "v2_replacement", "v3_replacement"]).
-define(UAP_FIELDS_OS, ["os_replacement", "os_v1_replacement", "os_v2_replacement", "os_v3_replacement", "os_v4_replacement"]).
-define(UAP_FIELDS_DEVICE, ["device_replacement", "brand_replacement", "model_replacement"]).
-define(UAP_MAP, [{"user_agent_parsers",?UAP_FIELDS_UA},{"os_parsers",?UAP_FIELDS_OS},{"device_parsers",?UAP_FIELDS_DEVICE}]).

load({Source, Pointer}) when (Source == file orelse Source == string), is_list(Pointer) ->
	[YAML] = yamerl_constr:Source(Pointer),
	UAP = lists:map(fun({K,F}) ->
		Y = proplists:get_value(K, YAML),
		load2(F, Y)
	end, ?UAP_MAP),
	list_to_tuple([uap|UAP]).

load2(Fields, REs) ->
	lists:map(fun(PL) -> load3(Fields, PL) end, REs).

load3(Fields, PL) ->
	RE = proplists:get_value("regex", PL),
	Opts = case proplists:get_value("regex_flag", PL) of
		undefined ->
			[];
		"i" ->
			[caseless]
	end,
	RE2 = lists:foldr(fun ($\\, A) -> [$\\,$\\|A]; (X, A) -> [X|A] end, RE),
	{ok, MP} = re:compile(RE2, Opts),
	Replace = lists:map(fun(F) -> proplists:get_value(F, PL) end, Fields),
	#uap_re{ re = MP, replace = Replace }.

parse(UA, UAP) when is_list(UA), is_record(UAP, uap) ->
	parse(UA, UAP, [ua,os,device]).
parse(UA, UAP, Order) when is_list(UA), is_record(UAP, uap), is_list(Order) ->
	Map = lists:map(fun(X) -> element(X, UAP) end, lists:map(fun uap_pos/1, Order)),
	Results = lists:map(fun(X) -> parse2(UA, ["Other"], X) end, Map),
	lists:map(fun parse4/1, lists:zip(Order, Results)).

parse2(_UA, Default, []) ->
	Default;
parse2(UA, Default, [RE = #uap_re{ re = MP }|REs]) ->
	Match = re:run(UA, MP, [{capture,all_but_first,list}]),
	parse3(UA, Default, REs, RE, Match).

parse3(_UA, _Default, _REs, #uap_re{ replace = Replace }, {match,Captured}) ->
io:format("~p~n", [{Captured, Replace}]),
	Replace2 = lists:zip(Replace, lists:seq(1, length(Replace))),
	lists:map(fun(X) -> replace(X, Captured) end, Replace2);
parse3(UA, Default, REs, _RE, nomatch) ->
	parse2(UA, Default, REs).

-define(PARSE4(X,Y), parse4({X,L}) ->
	L2 = L ++ lists:duplicate(record_info(size, Y) - 1 - length(L), undefined),
	list_to_tuple([Y|L2])).
?PARSE4(ua, uap_ua);
?PARSE4(os, uap_os);
?PARSE4(device, uap_device).

uap_pos(ua) -> #uap.ua;
uap_pos(os) -> #uap.os;
uap_pos(device) -> #uap.device.

replace({undefined, N}, Captured) when N > length(Captured) ->
	undefined;
replace({undefined, N}, Captured) ->
	lists:nth(N, Captured);
replace({R, _N}, Captured) ->
	replace2(R, Captured, []).

replace2([], _Captured, RN) ->
	RN2 = lists:dropwhile(fun(X) -> X < $! end, RN),
	lists:reverse(lists:dropwhile(fun(X) -> X < $! end, lists:reverse(RN2)));
replace2([$$,X|R], Captured, RN) when X >= $1, X =< $9 ->
	replace2(R, Captured, RN ++ lists:nth(X - $0, Captured));
replace2([X|R], Captured, RN) ->
	replace2(R, Captured, RN ++ [X]).
