-module(uap_normalize).

-export([process/1]).

-spec process(iolist()) -> iolist().
process(UA) ->
	Type = if is_binary(UA) -> binary; true -> list end,
	lists:foldl(fun
		({all,RE,R,O}, A) ->
			re:replace(A, RE, R, [unicode,ucp,{return,Type}|O]);
		({M,RE,R,O}, A) ->
			Match = string:find(A, M) =/= nomatch,
			if
				Match ->
					re:replace(A, RE, R, [unicode,ucp,{return,Type}|O]);
				true ->
					A
			end
	end, UA, [
		{all, "(?<=\\d)(?:\\.\\d+){2}", ".0.0", [global]},				% version
		{all, "(?<=\\d)(?:\\.\\d+){3}", ".0.0.0", [global]},				% version
		{all, "; [a-z]{2}[_-][A-Za-z]{2}(?=\\)|;)", "", []},				% 'en-US' langs
		{"BlackBerry", "; [a-z]{2}\\)", ")", []},					% 'en' lang
		{"Android", "(\\(Linux; (?:U; )?Android [\\d\\.]+); [^;\\)]+", "\\1", []},	% device name
		{" like ", "(OS \\d+)(?=_)(?:_\\d+)*", "\\1_0", [global]},			% iOS version
		{all, "(?<=[ /(])[\\dA-Fa-f-]{4,}(?![/.;\\dA-Fa-f-])", "0", [global]},		% user tracking
		{".NET CLR", "; \\.NET CLR [^;)]+", "", [global]},
		{";FBAV/", "(?<=\\[FB).*?(;FBAV/\\d+)[^\\]]*(?=\\])", "\\1.0.0.0.0", []},
		{"SpeedMode", "(SpeedMode;.*); .*(?=\\))", "\\1", []}
	]).
