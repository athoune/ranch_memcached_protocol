-module(rmp_dummy_plain).

-export([data/5, text/5]).

parseInt(S) ->
    {I, _} = string:to_integer(binary:bin_to_list(S)),
    I.

text(<<set>>, [Key, _Flags, _Exptime, Bytes], Context, _Socket, _Transport) ->
    Context2 = proplists:compact([{key, Key} | Context]),
    {data, parseInt(Bytes), Context2}.

data(set, Data, Context, _Socket, _Transport) ->
    Key = proplists:get_value(key, Context),
    io:format("set ~p : ~p~n", [Key, Data]).
