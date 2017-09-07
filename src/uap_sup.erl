-module(uap_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

start_link() ->
	start_link([]).
start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
	Procs = [#{ id => uap_server, start => {uap_server, start_link, [Args]} }],
	{ok, {#{}, Procs}}.
