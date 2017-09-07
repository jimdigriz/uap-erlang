-module(uap_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

start_link() ->
	start_link([]).
start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
	{ok, Name} = application:get_application(),
	Env = application:get_env(Name, uap_server, []),
	Procs = [#{ id => uap_server, start => {uap_server, start_link, [Env ++ Args]} }],
	{ok, {#{}, Procs}}.
