-module(uap_normalize_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("src/uap.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([normalize/1]).

all() ->
	[normalize].

init_per_suite(Config) ->
	application:ensure_all_started(yamerl),
	DataDir = ?config(data_dir, Config),
	{ok, UAP} = uap:state(file, filename:join([DataDir, "regexes.yaml"])),
	[{uap,UAP}|Config].

end_per_suite(_Config) ->
	ok.

normalize(Config) ->
	DataDir = ?config(data_dir, Config),
	{ok, IoDevice} = file:open(filename:join([DataDir, "list.gz"]), [binary,{encoding,unicode},compressed,{read_ahead,64*1024}]),
	normalize(Config, IoDevice, 0, 0, sets:new(), io:get_line(IoDevice, [])).

%%

normalize(Config, IoDevice, Count, Mismatch, Normalized0, eof) ->
	DataDir = ?config(data_dir, Config),
	ok = file:close(IoDevice),
	Normalized = sets:fold(fun(X, A) -> [X,$\n|A] end, [], Normalized0),
	ok = file:write_file(filename:join([DataDir, "list.norm.gz"]), Normalized, [{encoding,unicode},compressed]),
	NormalizedSize = sets:size(Normalized0),
	ct:log(info, ?STD_IMPORTANCE, "~b user agents normalized to ~b (mismatched ~p)", [Count, NormalizedSize, Mismatch]),
	true = NormalizedSize < (Count div 3) andalso Mismatch < ((NormalizedSize * 5) div 100);
normalize(Config, IoDevice, Count, Mismatch0, Normalized0, UA0) when is_binary(UA0) ->
	UAP = ?config(uap, Config),
	UA = string:trim(UA0, trailing),
	UAN = uap_normalize:process(UA),
	Match = if
		UAN =/= UA ->
			UAP_ParseN = mask(uap:parse(UAN, [ua,os], UAP)),
			UAP_Parse = mask(uap:parse(UA, [ua,os], UAP)),
			if
				UAP_ParseN =/= UAP_Parse ->
					ct:log(error, ?HI_IMPORTANCE, "'~s' changed~n'~s' when normalised~nwas: ~p~ngot: ~p~n", [UA, UAN, UAP_Parse, UAP_ParseN]),
					false;
				true ->
					true
			end;
		true ->
			true
	end,
	Normalized = sets:add_element(UAN, Normalized0),
	Mismatch = if Match -> Mismatch0; true -> Mismatch0 + 1 end,
	normalize(Config, IoDevice, Count + 1, Mismatch, Normalized, io:get_line(IoDevice, <<>>)).

mask(R) when is_list(R) ->
	lists:map(fun mask/1, R);
mask(R) when is_record(R, uap_ua) ->
	R#uap_ua{ minor = undefined, patch = undefined };
mask(R) when is_record(R, uap_os) ->
	R#uap_os{ minor = undefined, patch = undefined, patch_minor = undefined }.
