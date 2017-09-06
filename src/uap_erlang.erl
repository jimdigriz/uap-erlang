-module(uap_erlang).

-export([load/1]).
-export([parse/2]).

-include("uap.hrl").

-record(uap_re, {
	re	:: any(),
	replace	:: list()
}).

-define(UAP_FIELDS_UA, ["family_replacement", "v1_replacement", "v2_replacement", "v3_replacement"]).
-define(UAP_FIELDS_OS, ["os_replacement", "os_v1_replacement", "os_v2_replacement", "os_v3_replacement", "os_v4_replacement"]).
-define(UAP_FIELDS_DEVICE, ["device_replacement", "brand_replacement", "model_replacement"]).

load({Source, Pointer}) when (Source == file orelse Source == string) ->
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
	Results = lists:map(fun(X) ->
		{UA, Match} = lists:foldl(fun parse2/2, {UA, ["Other"]}, X),
		Match
	end, UAP),
	lists:zip([ua, os, device], Results).

parse2(_RE, A = {_UA, [Family|_]}) when Family =/= "Other" ->
	A;
parse2(RE = #uap_re{ re = MP }, A = {UA, _Match}) ->
	parse3(RE, A, re:run(UA, MP, [{capture,all_but_first,list}])).

parse3(#uap_re{ replace = _Replace }, {UA, _Match}, {match, Captured}) ->
	{UA, Captured};
parse3(_RE, A, nomatch) ->
	A.
