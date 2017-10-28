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

-record(cache, {
	key		:: {list() | binary()},
	ua		:: uap_ua(),
	os		:: uap_os(),
	device		:: uap_device()
}).

-define(DEFAULT_PRIV, uap).
-define(DEFAULT_FILE, "regexes.yaml").
-define(DEFAULT_CACHE, 1000).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
	application:start(yamerl),
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

-spec parse(list() | binary()) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA) when is_list(UA); is_binary(UA) ->
	parse(UA, [ua,os,device]).
-spec parse(list() | binary(), list(ua | os | device)) -> list(uap_ua() | uap_os() | uap_device()).
parse(UA, Order) when (is_list(UA) orelse is_binary(UA)), is_list(Order) ->
	parse2(UA, Order).

%% gen_server.

init(Args) ->
	Priv = proplists:get_value(priv, Args, ?DEFAULT_PRIV),
	File = proplists:get_value(file, Args, ?DEFAULT_FILE),
	CacheSize = proplists:get_value(cache, Args, ?DEFAULT_CACHE),
	ets:new(?MODULE, [named_table,{keypos,#cache.key},{read_concurrency,true}]),
	{ok, UAP} = uap:state({file,filename:join([code:priv_dir(Priv), File])}),
	{ok, #state{ uap = UAP, cache_size = CacheSize }}.

handle_call({parse, UA, Order}, _From, State = #state{ uap = UAP }) ->
	Result = uap:parse(UA, UAP, Order),
	Result2 = lists:zip(Order, Result),
	CacheSize = ets:info(?MODULE, size),
	ok = cache(UA, Result2, CacheSize, State),
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

cache(_UA, _Result, _CacheSize, #state{ cache_size = 0 }) ->
	ok;
cache(UA, Result, CacheSize, #state{ cache_size = X }) when X == unlimited; CacheSize < X ->
	Result2 = lists:map(fun({O,R}) -> {pos(O), R} end, Result),
	ets:insert_new(?MODULE, #cache{ key = UA }),
	true = ets:update_element(?MODULE, UA, Result2),
	ok;
cache(UA, Result, CacheSize, State) ->
	true = ets:delete(?MODULE, rkey(CacheSize)),
	cache(UA, Result, CacheSize - 1, State).

% http://erlang.org/pipermail/erlang-questions/2010-August/053051.html
rkey(CacheSize) ->
	I = rand:uniform(CacheSize),
	case I > CacheSize/2 of
		true ->
			rkey(I, ets:last(?MODULE), prev);
		false ->
			rkey(I, ets:first(?MODULE), next)
	end.
rkey(0, K, _Direction) -> K;
rkey(N, K, Direction) -> rkey(N - 1, ets:Direction(?MODULE, K), Direction).

parse2(UA, Order) ->
	Match = #cache{ key = UA, _ = '_' },
	{Match2, _} = lists:foldl(fun(O, {M,P}) ->
		M2 = setelement(pos(O), M, list_to_atom("$" ++ integer_to_list(P))),
		P2 = P + 1,
		{M2, P2}
	end, {Match, 1}, Order),
	case ets:match(?MODULE, Match2) of
		[] ->
			gen_server:call(?MODULE, {parse,UA,Order});
		[Result] ->
			Result2 = lists:zip(Order, Result),
			lists:map(fun
				({O,undefined}) ->
					[R] = gen_server:call(?MODULE, {parse,UA,[O]}),
					R;
				({_O,R}) ->
					R
			end, Result2)
	end.

pos(ua) -> #cache.ua;
pos(os) -> #cache.os;
pos(device) -> #cache.device.
