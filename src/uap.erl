-module(uap).

-export([state/2]).
-export([parse/2, parse/3]).

-include("uap.hrl").

-record(uap, {
	ua	:: uap_re(),
	os	:: uap_re(),
	device	:: uap_re()
}).

-type uap() :: #uap{}.

-record(uap_re, {
	re	:: tuple(),
	replace	:: list()
}).

-type uap_re() :: #uap_re{}.

-define(UAP_FIELDS_UA, ["family_replacement", "v1_replacement", "v2_replacement", "v3_replacement"]).
-define(UAP_FIELDS_OS, ["os_replacement", "os_v1_replacement", "os_v2_replacement", "os_v3_replacement", "os_v4_replacement"]).
-define(UAP_FIELDS_DEVICE, ["device_replacement", "brand_replacement", "model_replacement"]).
-define(UAP_MAP, [{"user_agent_parsers",?UAP_FIELDS_UA},{"os_parsers",?UAP_FIELDS_OS},{"device_parsers",?UAP_FIELDS_DEVICE}]).

-spec state(file | string, iolist()) -> {ok, uap()}.
state(Source, Pointer) when Source == file; Source == string ->
	[YAML] = yamerl_constr:Source(Pointer),
	UAP = lists:map(fun({K,F}) ->
		REs = proplists:get_value(K, YAML),
		lists:map(fun(PL) ->
			RE = proplists:get_value("regex", PL),
			Opts0 = case proplists:get_value("regex_flag", PL) of
				undefined ->
					[];
				"i" ->
					[caseless]
			end,
			{ok, MP} = re:compile(RE, [unicode,ucp|Opts0]),
			Replace = lists:map(fun(FF) ->
				proplists:get_value(FF, PL)
			end, F),
			#uap_re{ re = MP, replace = Replace }
		end, REs)
	end, ?UAP_MAP),
	{ok, list_to_tuple([uap|UAP])}.

-spec parse(iolist(), uap()) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA, UAP) when is_record(UAP, uap) ->
	parse(UA, [ua, os, device], UAP).

-spec parse(iolist(), list(ua | os | device), uap()) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA, Order, UAP) when is_record(UAP, uap) ->
	Other = if is_binary(UA) -> <<"Other">>; true -> "Other" end,
	lists:map(fun(Type) ->
		REs = element(uap_pos(Type), UAP),
		Result0 = parse2(UA, Type, REs),
		Result1 = if Result0 == nomatch -> [Other]; true -> Result0 end,
		Result2 = [uap_type(Type)|Result1] ++ [undefined,undefined,undefined,undefined],
		list_to_tuple(lists:sublist(Result2, uap_size(Type)))
	end, Order).

%%

uap_pos(ua) -> #uap.ua;
uap_pos(os) -> #uap.os;
uap_pos(device) -> #uap.device.

uap_type(ua) -> uap_ua;
uap_type(os) -> uap_os;
uap_type(device) -> uap_device.

uap_size(ua) -> record_info(size, uap_ua);
uap_size(os) -> record_info(size, uap_os);
uap_size(device) -> record_info(size, uap_device).

parse2(_UA, _Type, []) ->
	nomatch;
parse2(UA, Type, [RE = #uap_re{ re = MP }|REs]) ->
	Match = re:run(UA, MP, [{capture,all_but_first,list}]),
	parse3(UA, Type, REs, RE, Match).

parse3(UA, Type, _REs, RE, {match, Captured}) ->
	MatchDefault = if
		Type == device ->
			[1, 10, 1];	% 10 to make it impossible to match
		true ->
			lists:seq(1, length(RE#uap_re.replace))
	end,
	ReplacePairs = lists:zip(RE#uap_re.replace, MatchDefault),
	lists:map(fun
		(X) when is_binary(UA) ->
			to_binary(replace(X, Captured));
		(X) ->
			replace(X, Captured)
	end, ReplacePairs);
parse3(UA, Type, REs, _RE, nomatch) ->
	parse2(UA, Type, REs).

to_binary(X) when is_list(X) ->
	unicode:characters_to_binary(X);
to_binary(X) ->
	X.

replace({undefined, N}, Captured) when N > length(Captured) ->
	undefined;
replace({undefined, N}, Captured) ->
	lists:nth(N, Captured);
replace({R, _N}, Captured) ->
	replace2(R, Captured, []).

replace2([], _Captured, RN) ->
	case string:trim(lists:reverse(RN)) of [] -> undefined; X -> X end;
replace2([$$,X|R], Captured, RN) when X >= $1, X =< $9 ->
	O = X - $0,
	RNN = if O > length(Captured) -> []; true -> lists:nth(O, Captured) end,
	replace2(R, Captured, [RNN|RN]);
replace2([X|R], Captured, RN) ->
	replace2(R, Captured, [X|RN]).
