-module(rmp_dummy_plain).

-export([data/5, text/5]).

parseInt(S) ->
    {I, _} = string:to_integer(binary:bin_to_list(S)),
    I.

fromInt(I) ->
    binary:list_to_bin(io_lib:format("~p", [I])). % It looks very ugly

text(<<"get">>, Keys, _Context, Socket, Transport) ->
    lists:foreach(fun(Key) ->
                io:format("Key ~p~n", [Key]),
                Value = <<"popo">>,
                Bytes = fromInt(size(Value)),
                Transport:send(Socket, <<"VALUE ", Key/binary, " 0 ", Bytes/binary, 13, 10, Value/binary, 13, 10>>)
        end, Keys),
    Transport:send(Socket, <<"END", 13, 10>>),
    text;

text(<<"set">>, [Key, _Flags, _Exptime, Bytes], Context, _Socket, _Transport) ->
    Context2 = proplists:compact([{key, Key} | Context]),
    {data, set, parseInt(Bytes), Context2}.

data(set, Data, Context, Socket, Transport) ->
    Key = proplists:get_value(key, Context),
    io:format("set ~p : ~p~n", [Key, Data]),
    Transport:send(Socket, <<"STORED", 13, 10>>),
    text.
