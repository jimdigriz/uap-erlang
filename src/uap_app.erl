-module(uap_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, Args) ->
	{ok, Priv} = application:get_env(uap,priv),
	{ok, File} = application:get_env(uap,file),
	uap_sup:start_link([{priv,Priv},{file,File}|Args]).

stop(_State) ->
	ok.
