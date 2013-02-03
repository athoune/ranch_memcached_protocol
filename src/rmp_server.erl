-module(rmp_server).

-export([data/7, text/7]).

parseInt(S) ->
    {I, _} = string:to_integer(binary:bin_to_list(S)),
    I.

fromInt(I) ->
    binary:list_to_bin(io_lib:format("~p", [I])). % It looks very ugly

text(<<"get">>, Keys, _Context, Socket, Transport, Handler, Opts) ->
    lists:foreach(fun(Key) ->
                case Handler:get(Key, Opts) of
                    {ok, Value, _} ->
                        Bytes = fromInt(size(Value)),
                        Transport:send(Socket, <<"VALUE ",
                            Key/binary, " 0 ", Bytes/binary, 13, 10,
                            Value/binary, 13, 10>>);
                    {none, _} ->
                        io:format("Nothing for ~p~n", [Key]),
                        none
                end
        end, Keys),
    Transport:send(Socket, <<"END", 13, 10>>),
    {text, Opts};

% [FIXME] handle noreply
text(<<"delete">>, [Key], _Context, Socket, Transport, Handler, Opts) ->
    case Handler:delete(Key, Opts) of
        {ok, Opts2} ->
            Transport:send(Socket, <<"DELETED", 13, 10>>);
        {none, Opts2} ->
            Transport:send(Socket, <<"NOT_FOUND", 13, 10>>)
    end,
    {text, Opts2};

text(<<"stats">>, [], _Context, Socket, Transport, Handler, Opts) ->
    lists:foreach(fun(Stat) ->
                Transport:send(Socket, <<"STAT ", Stat/binary, 13, 10>>)
        end, Handler:stats(Opts)),
    Transport:send(Socket, <<"END", 13, 10>>),
    {text, Opts};


text(<<"set">>, [Key, Flags, Exptime, Bytes], _Context, _Socket, _Transport, _Handler, Opts) ->
    {{data, set, parseInt(Bytes), [Key, Flags, Exptime, Bytes]}, Opts}.

data(set, Data, [Key, Flags, Exptime, _Bytes], Socket, Transport, Handler, Opts) ->
    {ok, Opts2} = Handler:set(Key, Flags, Exptime, Data, Opts),
    Transport:send(Socket, <<"STORED", 13, 10>>),
    {text, Opts2}.
