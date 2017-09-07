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
	uap,
	cache_size	:: unlimited | non_neg_integer()
}).

-define(DEFAULT_PRIV, uap).
-define(DEFAULT_FILE, "regexes.yaml").
-define(DEFAULT_CACHE, 1000).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

-spec parse(list() | binary()) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA) when is_list(UA); is_binary(UA) ->
	parse(UA, [ua,os,device]).
-spec parse(list() | binary(), list(ua | os | device)) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA, Order) when (is_list(UA) orelse is_binary(UA)), is_list(Order) ->
	lists:map(fun(X) -> parse2(UA, X) end, Order).

%% gen_server.

init(Args) ->
	Priv = proplists:get_value(priv, Args, ?DEFAULT_PRIV),
	File = proplists:get_value(file, Args, ?DEFAULT_FILE),
	CacheSize = proplists:get_value(cache, Args, ?DEFAULT_CACHE),
	{ok, UAP} = uap:state({file,filename:join([code:priv_dir(Priv), File])}),
	?MODULE = ets:new(?MODULE, [named_table,{read_concurrency,true}]),
	{ok, #state{ uap = UAP, cache_size = CacheSize }}.

handle_call({parse, UA, Order}, _From, State = #state{ uap = UAP }) ->
	Result = uap:parse(UA, UAP, Order),
	ok = cache(UA, Order, Result, ets:info(?MODULE, size), State),
	{reply, Result, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%

cache(_UA, _O, _Result, _CacheSize, #state{ cache_size = 0 }) ->
	ok;
cache(UA, O, Result, CacheSize, #state{ cache_size = X }) when X == unlimited; CacheSize < X ->
	true = ets:insert(?MODULE, {{O,UA},Result}),
	ok;
cache(UA, O, Result, CacheSize, State) ->
	true = ets:safe_fixtable(?MODULE, true),
	true = ets:delete(?MODULE, rkey(CacheSize)),
	true = ets:safe_fixtable(?MODULE, false),
	cache(UA, O, Result, CacheSize - 1, State).

% http://erlang.org/pipermail/erlang-questions/2010-August/053051.html
rkey(CacheSize) -> rkey(rand:uniform(CacheSize), ets:first(?MODULE)).
rkey(0, K) -> K;
rkey(N, K) -> rkey(N - 1, ets:next(?MODULE, K)).

parse2(UA, O) ->
	case ets:lookup(?MODULE, {[O],UA}) of
		[] ->
			[X] = gen_server:call(?MODULE, {parse,UA,[O]}),
			X;
		[{_,[X]}] ->
			X
	end.
