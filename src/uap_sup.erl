-module(uap_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [#{ id => uap_server, start => {uap_server,start_link,[]} }],
	{ok, {#{}, Procs}}.
