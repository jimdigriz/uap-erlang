-module(uap_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, Args) ->
	Args2 = lists:foldl(fun(X, A) ->
		case application:get_env(uap, X) of
			{ok, Y} ->
				[{X,Y}|A];
			undefined ->
				A
		end
	end, Args, [priv, file, cache]),
	uap_sup:start_link(Args2).

stop(_State) ->
	ok.
