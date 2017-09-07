-module(uap_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, Args) ->
	uap_sup:start_link(Args).

stop(_State) ->
	ok.
