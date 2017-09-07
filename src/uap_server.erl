-module(uap_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	uap
}).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

%% gen_server.

init(Args) ->
	Priv = proplists:get_value(priv, Args, uap),
	File = proplists:get_value(file, Args),
	{ok, UAP} = uap:state({file,filename:join([code:priv_dir(Priv),File])}),
	{ok, #state{ uap = UAP }}.

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
