-module(ranch_memcached_protocol_plain).
-export([start_link/4, init/4]).


start_link(ListenerPid, Socket, Transport, [Handler]) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, [Handler]]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(ListenerPid),
    io:format("Got a connection!~n"),
    loop(text, Socket, Transport, Opts, <<>>),
    ok.

loop(text, Socket, Transport, [Handler]=Opts, Remains) ->
    {ok, Line, Remains2} = read_line(Socket, Transport, Remains),
    io:format("line ~p~n", [Line]),
    [Command|Args] = binary:split(Line, <<32>>, [global]),
    R = Handler:text(Command, Args, [], Socket, Transport),
    io:format("texte red: ~p~n", [R]),
    loop(R, Socket, Transport, Opts, Remains2);

loop({data, Command, Size, Context}, Socket, Transport, [Handler]=Opts, Remains) ->
    {ok, <<Data:Size/binary, 13, 10>>, Remains2} = read(Size + 2, Socket, Transport, Remains),
    R = Handler:data(Command, Data, Context, Socket, Transport),
    loop(R, Socket, Transport, Opts, Remains2).

read(Length, Socket, Transport, Remains) ->
    case size(Remains) >= Length of
        true ->
            <<Data:Length/binary, Remains2/binary>> = Remains,
            {ok, Data, Remains2};
        false ->
            case Transport:recv(Socket, Length-size(Remains), 30000) of
                {ok, Data} ->
                    L = size(Data),
                    case L of
                        Length -> {ok,  <<Remains, Data>>, <<>>};
                            _ -> read(Length-L, Socket, Transport, <<Remains, Data>>)
                    end;
                Error -> Error
            end
    end.

try_to_split(<<>>) ->
    again;

try_to_split(Blob) ->
    case binary:split(Blob, <<13,10>>) of
        [Line, Remains2] ->
            {Line, Remains2};
        [_Remains2] ->
            again
    end.

read_line(Socket, Transport, Remains) ->
    io:format("Stuff: ~p~n", [Remains]),
    case try_to_split(Remains) of
        again ->
            case Transport:recv(Socket, 0, 3000) of
                {ok, Data} ->
                    io:format("Slurping: ~p~n", [Data]),
                    Blob = <<Remains/binary, Data/binary>>,
                    read_line(Socket, Transport, Blob);
                Error -> Error
            end;
        {Line, Remains2} ->
            {ok, Line, Remains2}
    end.
