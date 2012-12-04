-module(ranch_memcached_protocol).
-export([start_link/4, init/4, respond/3]).

-include("rmp_constants.hrl").

-record(header, {
    extra,
    key,
    body,
    total,
    opcode,
    opaque,
    cas
    }).


-record(opts, {
    handler=dummy
    }).

start_link(ListenerPid, Socket, Transport, [Handler]) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, #opts{handler=Handler}]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(ListenerPid),
    io:format("Got a connection!~n"),
    loop(header, Socket, Transport, Opts),
    ok.

read(Length, Socket, Transport) ->
    case Transport:recv(Socket, Length, 30000) of
        {ok, Data} ->
            L = size(Data),
            case L of
                Length -> {ok, Data};
                    _ -> read(Length-L, Socket, Transport, Data)
            end;
        Error -> Error
    end.

read(Length, Socket, Transport, Remains) ->
    case Transport:recv(Socket, Length, 30000) of
        {ok, Data} ->
            L = size(Data),
            case L of
                Length -> {ok,  <<Remains, Data>>};
                    _ -> read(Length-L, Socket, Transport, <<Remains, Data>>)
            end;
        Error -> Error
    end.


loop(header, Socket, Transport, Opts) ->
    case read(24, Socket, Transport) of
        {ok, Data} ->
            {ok, Header} = handle_header(Data),
            loop(Header, Socket, Transport, Opts);
        {error, Error} ->
            io:format("Socket error : ~p~n", [Error])
    end;
loop(#header{total=Len}=Sizes, Socket, Transport, Opts) ->
    {ok, Data} = read(Len, Socket, Transport),
    handle_body({Socket, Transport}, Data, Sizes, Opts),
    loop(header, Socket, Transport, Opts).

handle_header(<<?REQ_MAGIC:8, Opcode:8, KeyLen:16,
    ExtraLen:8, 0:8, 0:16,
    BodyLen:32,
    Opaque:32,
    CAS:64>>) ->
    {ok, #header{extra=ExtraLen, key=KeyLen, body=BodyLen - (KeyLen + ExtraLen),
            total=BodyLen, opcode=Opcode, opaque=Opaque, cas=CAS}}.

handle_body(Conn, Data, #header{extra=ExtraLen, key=KeyLen, body=BodyLen,
        opcode=Opcode, opaque=Opaque, cas=CAS}, #opts{handler=Handler}) ->
    EL = ExtraLen * 8,
    KL = KeyLen * 8,
    BL = BodyLen * 8,
    <<Extra:EL/bitstring, Key:KL/bitstring, Body:BL/bitstring>> = Data,
    Message = #rmp_message{extra=Extra, key=Key, body=Body, opaque=Opaque, cas=CAS},
    io:format("Message ~p ~p~n", [Opcode, Message]),
    io:format("Handler ~p~n", [Handler]),
    Handler:handle(Conn, Opcode, Message),
    ok.


bin_size(undefined) -> 0;
bin_size(List) when is_list(List) -> bin_size(list_to_binary(List));
bin_size(Binary) -> size(Binary).

xmit(_, undefined) -> ok;
xmit(Conn, List) when is_list(List) -> xmit(Conn, list_to_binary(List));
xmit({Socket, Transport}, Data) -> io:format("xmit ~p~n", [Data]), Transport:send(Socket, Data).

respond({Socket, Transport}=Conn, OpCode,
        #rmp_response{extra=Extra, key=Key, body=Body, status=Status, cas=CAS, opaque=Opaque}) ->
    KeyLen = bin_size(Key),
    ExtraLen = bin_size(Extra),
    BodyLen = bin_size(Body) + (KeyLen + ExtraLen),
    io:format("Respond ~p ~n", [[OpCode, KeyLen, ExtraLen, Status, BodyLen, Opaque, CAS]]),
    Blob = <<?RES_MAGIC, OpCode:8, KeyLen:16,
                               ExtraLen:8, 0:8, Status:16,
                               BodyLen:32, Opaque:32, CAS:64>>,
    io:format("Blob ~p~n", [Blob]),
    ok = Transport:send(Socket, Blob),
    ok = xmit(Conn, Extra),
    ok = xmit(Conn, Key),
    ok = xmit(Conn, Body).
