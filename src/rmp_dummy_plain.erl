-module(rmp_dummy_plain).
-behaviour(gen_memcached).

-export([get/2, set/5]).

get(Key, Opts) ->
    {ok, <<"popo">>, Opts}.

set(Key, _Flags, _Exptime, Data, Opts) ->
    io:format("set ~p : ~p~n", [Key, Data]),
    {ok, Opts}.
