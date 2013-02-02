-module(rmp_dummy_plain).

-export([get/2, set/5]).

get(Key, Opts) ->
    {ok, <<"popo">>, Opts}.

set(Key, _Flags, _Exptime, Data, Opts) ->
    io:format("set ~p : ~p~n", [Key, Data]),
    {ok, Opts}.
