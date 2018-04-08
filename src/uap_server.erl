-module(uap_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([parse/1, parse/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(PARTITIONS, 10).

-include("uap.hrl").

-record(state, {
	uap				:: uap:uap(),
	cache_size			:: unlimited | non_neg_integer(),

	requests	= dict:new()	:: dict:dict({binary(), list(ua | os | device)}, list(any())),
	monitors	= dict:new()	:: dict:dict(pid(), {reference(), binary()})
}).

-record(cache, {
	key				:: {non_neg_integer(), binary()},
	ua				:: uap_ua(),
	os				:: uap_os(),
	device				:: uap_device()
}).

-define(DEFAULT_PRIV, uap).
-define(DEFAULT_FILE, "regexes.yaml").
-define(DEFAULT_CACHE, 1000).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

-spec parse(iodata()) -> {ok, list(uap_ua() | uap_os() | uap_device())} | {error,any()}.
parse(UA) ->
	parse(UA, [ua, os, device]).
-spec parse(iodata(), list(ua | os | device)) -> {ok, list(uap_ua() | uap_os() | uap_device())} | {error, any()}.
parse(UA0, Order) when is_list(UA0), is_list(Order) ->	% binary re is faster
	UA = unicode:characters_to_binary(UA0),
	case parse(UA, Order) of
		{ok, Result0} ->
			Result = lists:map(fun(R) ->
				list_to_tuple(lists:map(fun
					(X) when is_binary(X) ->
						unicode:characters_to_list(X);
					(X) ->
						X
				end, tuple_to_list(R)))
			end, Result0),
			{ok, Result};
		Else ->
			Else
	end;
parse(UA, Order) when is_binary(UA), is_list(Order), length(Order) > 0 ->
	Valid = length(Order) == length(lists:usort(Order)),
	if Valid -> parse2(UA, Order, ets:lookup(?MODULE, key(UA))); true -> {error,duplicate} end.

%% gen_server.

init(Args) ->
	Priv = proplists:get_value(priv, Args, ?DEFAULT_PRIV),
	File = proplists:get_value(file, Args, ?DEFAULT_FILE),
	CacheSize = proplists:get_value(cache, Args, ?DEFAULT_CACHE),
	ets:new(?MODULE, [named_table,ordered_set,{keypos,#cache.key},{read_concurrency,true}]),
	{ok, UAP} = uap:state(file, filename:join([code:priv_dir(Priv), File])),
	{ok, #state{ uap = UAP, cache_size = CacheSize }}.

handle_call({parse, UA, Order}, From, State) ->
	Key = {UA, Order},
	Duplicate = dict:is_key(Key, State#state.requests),
	Monitors = if
		Duplicate ->
			State#state.monitors;
		true ->
			{Pid, MonitorRef} = spawn_monitor(fun() ->
				Result = uap:parse(UA, Order, State#state.uap),
				ok = gen_server:cast(?MODULE, {parse, self(), UA, Order, Result})
			end),
			dict:store(Pid, {MonitorRef, Key}, State#state.monitors)
	end,
	Requests = dict:append(Key, From, State#state.requests),
	{noreply, State#state{ requests = Requests, monitors = Monitors }};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({parse, Pid, UA, Order, Result}, State) ->
	Key = {UA, Order},
	{Froms, Requests} = dict:take(Key, State#state.requests),
	lists:foreach(fun(From) -> gen_server:reply(From, {ok, Result}) end, Froms),
	{{MonitorRef, Key}, Monitors} = dict:take(Pid, State#state.monitors),
	demonitor(MonitorRef, [flush]),
	cache({UA, Result}, State),
	{noreply, State#state{ requests = Requests, monitors = Monitors }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Info}, State) when Info =/= normal ->
	{{MonitorRef, Key}, Monitors} = dict:take(Pid, State#state.monitors),
	{Froms, Requests} = dict:take(Key, State#state.requests),
	lists:foreach(fun(From) -> gen_server:reply(From, {error,Info}) end, Froms),
	{noreply, State#state{ requests = Requests, monitors = Monitors }};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%

% http://erlang.2086793.n4.nabble.com/ranged-lookup-on-ordered-set-ETS-td3717828.html
% ordered_set so the match_delete in effect becomes a ranged delete (2x faster)
key(UA) ->
	{erlang:phash2(UA, ?PARTITIONS), UA}.

parse2(UA, Order, []) ->
	parse_real(UA, Order);
parse2(UA, Order, [ResultCache]) ->
	Tuple = {_ResultCachePairs, {_Complete, _OrderMissing}} = lists:mapfoldl(fun(T, {C0,O0}) ->
		R = element(cache_pos(T), ResultCache),
		X = {_C, _O} = if
			R == undefined ->
				{false, [T|O0]};
			true ->
				{C0, O0}
		end,
		{{T, R}, X}
	end, {true, []}, Order),
	parse3(UA, Order, Tuple).

parse3(_UA, _Order, {ResultCachePairs, {true, _OrderMissing}}) ->
	{_, Result} = lists:unzip(ResultCachePairs),
	{ok, Result};
parse3(UA, _Order, {ResultCachePairs, {false, OrderMissing}}) ->
	case parse_real(UA, OrderMissing) of
		{ok, ResultParse} ->
			Result = lists:foldr(fun
				({T, undefined}, R) ->
					{value, X} = lists:keysearch(uap_type(T), 1, ResultParse),
					[X|R];
				({_T, X}, R) ->
					[X|R]
			end, [], ResultCachePairs),
			{ok, Result};
		Else ->
			Else
	end.

parse_real(UA, Order) ->
	try gen_server:call(?MODULE, {parse, UA, Order}) of
		Result ->
			Result
	catch
		_:Error ->
			{error,Error}
	end.

cache_pos(ua) -> #cache.ua;
cache_pos(os) -> #cache.os;
cache_pos(device) -> #cache.device.

uap_type(ua) -> uap_ua;
uap_type(os) -> uap_os;
uap_type(device) -> uap_device.

cache({_UA, _Result}, _State = #state{ cache_size = 0 }) ->
	ok;
cache({UA, Result}, _State = #state{ cache_size = unlimited }) ->
	Key = key(UA),
	{ElementSpec, Cache} = lists:mapfoldl(fun
		(R = #uap_ua{}, C) ->
			{{#cache.ua, R}, C#cache{ ua = R }};
		(R = #uap_os{}, C) ->
			{{#cache.os, R}, C#cache{ os = R }};
		(R = #uap_device{}, C) ->
			{{#cache.device, R}, C#cache{ device = R }}
	end, #cache{ key = Key }, Result),
	Inserted = ets:insert_new(?MODULE, Cache),
	if
		not Inserted ->
			ets:update_element(?MODULE, Key, ElementSpec);
		true ->
			ok
	end;
cache(X = {_UA, _Result}, State) ->
	Size = ets:info(?MODULE, size),
	if
		Size >= State#state.cache_size ->
			Partition = rand:uniform(?PARTITIONS) - 1,
			true = ets:match_delete(?MODULE, #cache{ key = {Partition,'_'}, _ = '_' });
		true ->
			ok
	end,
	cache(X, State#state{ cache_size = unlimited }).
