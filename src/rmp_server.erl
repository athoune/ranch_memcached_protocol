-module(rmp_server).

-export([data/7, text/7]).

parseInt(S) ->
    {I, _} = string:to_integer(binary:bin_to_list(S)),
    I.

fromInt(I) ->
    binary:list_to_bin(io_lib:format("~p", [I])). % It looks very ugly

text(<<"get">>, Keys, _Context, Socket, Transport, Handler, Opts) ->
    lists:foreach(fun(Key) ->
                io:format("Key ~p~n", [Key]),
                {ok, Value, _} = Handler:get(Key, Opts),
                Bytes = fromInt(size(Value)),
                Transport:send(Socket, <<"VALUE ", Key/binary, " 0 ", Bytes/binary, 13, 10, Value/binary, 13, 10>>)
        end, Keys),
    Transport:send(Socket, <<"END", 13, 10>>),
    text;

text(<<"set">>, [Key, Flags, Exptime, Bytes], _Context, _Socket, _Transport, _Handler, _Opts) ->
    {data, set, parseInt(Bytes), [Key, Flags, Exptime, Bytes]}.

data(set, Data, [Key, Flags, Exptime, _Bytes], Socket, Transport, Handler, Opts) ->
    {ok, _Opts2} = Handler:set(Key, Flags, Exptime, Data, Opts),
    Transport:send(Socket, <<"STORED", 13, 10>>),
    text.
