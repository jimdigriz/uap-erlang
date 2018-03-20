-module(uap).

-export([state/1]).
-export([parse/2, parse/3]).

-include("uap.hrl").

-record(uap, {
	ua	:: uap_re(),
	os	:: uap_re(),
	device	:: uap_re()
}).

-record(uap_re, {
	type	:: ua | os | device,
	re	:: tuple(),
	replace	:: list()
}).

-type uap_re() :: #uap_re{}.

-define(UAP_FIELDS_UA, ["family_replacement", "v1_replacement", "v2_replacement", "v3_replacement"]).
-define(UAP_FIELDS_OS, ["os_replacement", "os_v1_replacement", "os_v2_replacement", "os_v3_replacement", "os_v4_replacement"]).
-define(UAP_FIELDS_DEVICE, ["device_replacement", "brand_replacement", "model_replacement"]).
-define(UAP_MAP, [{"user_agent_parsers",?UAP_FIELDS_UA},{"os_parsers",?UAP_FIELDS_OS},{"device_parsers",?UAP_FIELDS_DEVICE}]).

state({Source, Pointer}) when (Source == file orelse Source == string), is_list(Pointer) ->
	[YAML] = yamerl_constr:Source(Pointer),
	UAP = lists:map(fun({K,F}) ->
		Y = proplists:get_value(K, YAML),
		state2(K, F, Y)
	end, ?UAP_MAP),
	{ok, list_to_tuple([uap|UAP])}.

state2(K, Fields, REs) ->
	lists:map(fun(PL) -> state3(K, Fields, PL) end, REs).

state3(K, Fields, PL) ->
	RE = proplists:get_value("regex", PL),
	Opts = case proplists:get_value("regex_flag", PL) of
		undefined ->
			[unicode,ucp];
		"i" ->
			[unicode,ucp,caseless]
	end,
	{ok, MP} = re:compile(RE, Opts),
	Replace = lists:map(fun(F) -> proplists:get_value(F, PL) end, Fields),
	Type = case K of "user_agent_parsers" -> ua; "os_parsers" -> os; "device_parsers" -> device end,
	#uap_re{ type = Type, re = MP, replace = Replace }.

parse(UA, UAP) when (is_list(UA) orelse is_binary(UA)), is_record(UAP, uap) ->
	parse(UA, UAP, [ua,os,device]).
parse(UA, UAP, Order) when is_binary(UA), is_record(UAP, uap), is_list(Order) ->
	lists:map(fun(X) ->
		lists:foldl(fun(I, E) ->
			setelement(I, E, str2bin(element(I, E)))
		end, X, lists:seq(2, size(X)))
	end, parse(binary_to_list(UA), UAP, Order));
parse(UA, UAP, Order) when is_list(UA), is_record(UAP, uap), is_list(Order) ->
	Map = lists:map(fun(X) -> element(X, UAP) end, lists:map(fun uap_pos/1, Order)),
	Results = lists:map(fun(X) -> parse2(UA, ["Other"], X) end, Map),
	lists:map(fun parse4/1, lists:zip(Order, Results)).

parse2(_UA, Default, []) ->
	Default;
parse2(UA, Default, [RE = #uap_re{ re = MP }|REs]) ->
	Match = re:run(UA, MP, [{capture,all_but_first,list}]),
	parse3(UA, Default, REs, RE, Match).

parse3(_UA, _Default, _REs, #uap_re{ replace = Replace, type = device }, {match,Captured}) ->
	Replace2 = lists:zip(Replace, [1, 10, 1]),	% 10 to make it impossible to match
	lists:map(fun(X) -> replace(X, Captured) end, Replace2);
parse3(_UA, _Default, _REs, #uap_re{ replace = Replace }, {match,Captured}) ->
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

str2bin(X) when is_list(X) -> unicode:characters_to_binary(X);
str2bin(X) -> X.

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
	case string:trim(RN) of [] -> undefined; X -> X end;
replace2([$$,X|R], Captured, RN) when X >= $1, X =< $9 ->
	O = X - $0,
	RNN = case O > length(Captured) of true -> []; false -> lists:nth(O, Captured) end,
	replace2(R, Captured, RN ++ RNN);
replace2([X|R], Captured, RN) ->
	replace2(R, Captured, RN ++ [X]).
