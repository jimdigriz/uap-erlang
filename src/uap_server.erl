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

-include("uap.hrl").

-record(state, {
	uap				:: uap:uap(),
	cache_size			:: unlimited | non_neg_integer(),

	requests	= dict:new()	:: dict:dict({binary(), list(ua | os | device)}, list(any())),
	monitors	= dict:new()	:: dict:dict(pid(), {reference(), binary()})
}).

-record(cache, {
	key				:: binary(),
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

-spec parse(iolist()) -> {ok, list(uap_ua() | uap_os() | uap_device())} | {error, any()}.
parse(UA) ->
	parse(UA, [ua, os, device]).
-spec parse(iolist(), list(ua | os | device)) -> {ok, list(uap_ua() | uap_os() | uap_device())} | {error, any()}.
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
parse(UA, Order) when is_binary(UA), is_list(Order) ->
	parse2(UA, Order).

%% gen_server.

init(Args) ->
	Priv = proplists:get_value(priv, Args, ?DEFAULT_PRIV),
	File = proplists:get_value(file, Args, ?DEFAULT_FILE),
	CacheSize = proplists:get_value(cache, Args, ?DEFAULT_CACHE),
	ets:new(?MODULE, [named_table,{keypos,#cache.key},{read_concurrency,true}]),
	{ok, UAP} = uap:state(file, filename:join([code:priv_dir(Priv), File])),
	{ok, #state{ uap = UAP, cache_size = CacheSize }}.

handle_call({parse, UA, Order}, From, State) ->
	Key = {UA, lists:usort(Order)},
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
	Key = {UA, lists:usort(Order)},
	{Froms, Requests} = dict:take(Key, State#state.requests),
	{{MonitorRef, Key}, Monitors} = dict:take(Pid, State#state.monitors),
	lists:foreach(fun(From) -> gen_server:reply(From, {ok, Result}) end, Froms),
	demonitor(MonitorRef, [flush]),
	cache({UA, Result}, State),
	{noreply, State#state{ requests = Requests, monitors = Monitors }};
handle_cast({cache_delete, Key}, State) ->
	true = ets:delete(?MODULE, Key),
	{noreply, State};
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

cache({_UA, _Result}, _State = #state{ cache_size = 0 }) ->
	ok;
cache({UA, Result}, _State = #state{ cache_size = unlimited }) ->
	{ElementSpec, Cache} = lists:mapfoldl(fun
		(R = #uap_ua{}, C) ->
			{{#cache.ua, R}, C#cache{ ua = R }};
		(R = #uap_os{}, C) ->
			{{#cache.os, R}, C#cache{ os = R }};
		(R = #uap_device{}, C) ->
			{{#cache.device, R}, C#cache{ device = R }}
	end, #cache{ key = UA }, Result),
	Inserted = ets:insert_new(?MODULE, Cache),
	if
		not Inserted ->
			ets:update_element(?MODULE, UA, ElementSpec);
		true ->
			ok
	end;
cache(X = {_UA, _Result}, State) ->
	spawn_link(fun() ->
		Size = ets:info(?MODULE, size),
		if
			Size >= State#state.cache_size ->
				Key = rkey(Size),
				gen_server:cast(?MODULE, {cache_delete, Key});
			true ->
				ok
		end
	end),
	cache(X, State#state{ cache_size = unlimited }).

% http://erlang.org/pipermail/erlang-questions/2010-August/053051.html
rkey(Size) ->
	true = ets:safe_fixtable(?MODULE, true),
	I = rand:uniform(Size),
	if
		I > Size div 2 ->
			rkey(ets:last(?MODULE), prev, I);
		true ->
			rkey(ets:first(?MODULE), next, I)
	end.
rkey(K, _Direction, 0) ->
	true = ets:safe_fixtable(?MODULE, false),
	K;
rkey(K, Direction, N) ->
	rkey(ets:Direction(?MODULE, K), Direction, N - 1).

parse2(UA, Order) ->
	case ets:lookup(?MODULE, UA) of
		[] ->
			parse3(UA, Order);
		[ResultCache] ->
			{ResultCachePairs, Complete} = lists:mapfoldl(fun(T, C) ->
				R = element(cache_pos(T), ResultCache),
				{{R, T}, C andalso R =/= undefined}
			end, true, Order),
			Result = if
				Complete ->
					{Result0, _} = lists:unzip(ResultCachePairs),
					Result0;
				true ->
					OrderMissing = lists:usort(lists:foldl(fun
						({undefined, T}, O) ->
							[T|O];
						(_, O) ->
							O
					end, [], ResultCachePairs)),
					ResultParse = parse3(UA, OrderMissing),
					lists:foldr(fun
						({undefined, T}, R) ->
							{value, X} = lists:keysearch(uap_type(T), 1, ResultParse),
							[X|R];
						({X, _T}, R) ->
							[X|R]
					end, [], ResultCachePairs)
			end,
			{ok, Result}
	end.

parse3(UA, Order) ->
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
