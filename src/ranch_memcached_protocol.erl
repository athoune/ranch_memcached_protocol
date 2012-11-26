-module(ranch_memcached_protocol).
-export([start_link/4, init/3]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport) ->
    ok = ranch:accept_ack(ListenerPid),
    io:format("Got a connection!~n"),
    ok.

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 30000) of
        {ok, Data} ->
            handle(Socket, Transport, Data),
            loop(Socket, Transport);
        {error, _} ->
            io:format("The client disconnected~n")
    end.
 
handle(Socket, Transport, Data) ->
    io:format("Command received ~p~n", [Data]),
    Transport:send(Socket, <<"500 Bad command\r\n">>).
