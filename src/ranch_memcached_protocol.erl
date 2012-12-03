-module(ranch_memcached_protocol).
-export([start_link/4, init/4, respond/4, handle/3]).

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

-record(message, {
    extra,
    key,
    body,
    cas,
    opaque
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
    Message = #message{extra=Extra, key=Key, body=Body, opaque=Opaque, cas=CAS},
    io:format("Message ~p~n", [Message]),
    io:format("Handler ~p~n", [Handler]),
    Handler:handle(Conn, Opcode, Message),
    ok.

handle(Conn, ?SET, #message{}=Message) ->
    io:format("Handle ~p~n", [Message]),
    respond(Conn, ?SUCCESS, 0, #rmp_response{});
handle(_Conn, Opcode, Message) ->
    io:format("Oups handle: ~p ~p ~n", [Opcode, Message]).

bin_size(undefined) -> 0;
bin_size(List) when is_list(List) -> bin_size(list_to_binary(List));
bin_size(Binary) -> size(Binary).

xmit(_, undefined) -> ok;
xmit(Conn, List) when is_list(List) -> xmit(Conn, list_to_binary(List));
xmit({Socket, Transport}, Data) -> Transport:send(Socket, Data).

respond({Socket, Transport}=Conn, OpCode, Opaque,
        #rmp_response{extra=Extra, key=Key, body=Body, status=Status, cas=CAS}) ->
    KeyLen = bin_size(Key),
    ExtraLen = bin_size(Extra),
    BodyLen = bin_size(Body) + (KeyLen + ExtraLen),
    %io:format("Respond ~p ~n", [[OpCode, KeyLen, ExtraLen, Status, BodyLen, Opaque, CAS]]),
    Blob = <<?RES_MAGIC, OpCode:8, KeyLen:16,
                               ExtraLen:8, 0:8, Status:16,
                               BodyLen:32, Opaque:32, CAS:64>>,
    ok = Transport:send(Socket, Blob),
    ok = xmit(Conn, Extra),
    ok = xmit(Conn, Key),
    ok = xmit(Conn, Body).

%process_message(Socket, StorageServer, {ok, <<?REQ_MAGIC:8, ?STAT:8, KeyLen:16,
                                            %ExtraLen:8, 0:8, 0:16,
                                            %BodyLen:32,
                                            %Opaque:32,
                                            %CAS:64>>}) ->
    %error_logger:info_msg("Got a stat request for ~p.~n", [StorageServer]),

    %Extra = read_data(Socket, ExtraLen, extra),
    %Key = read_data(Socket, KeyLen, key),
    %Body = read_data(Socket, BodyLen - (KeyLen + ExtraLen), body),

    %% Hand the request off to the server.
    %gen_server:cast(StorageServer, {?STAT, Extra, Key, Body, CAS, Socket, Opaque});
%process_message(Socket, StorageServer, {ok, <<?REQ_MAGIC:8, OpCode:8, KeyLen:16,
                                            %ExtraLen:8, 0:8, 0:16,
                                            %BodyLen:32,
                                            %Opaque:32,
                                            %CAS:64>>}) ->
    %error_logger:info_msg("Got message of type ~p to give to ~p.~n",
                          %[OpCode, StorageServer]),

    %Extra = read_data(Socket, ExtraLen, extra),
    %Key = read_data(Socket, KeyLen, key),
    %Body = read_data(Socket, BodyLen - (KeyLen + ExtraLen), body),

    %% Hand the request off to the server.
    %Res = gen_server:call(StorageServer, {OpCode, Extra, Key, Body, CAS}),

    %respond(Socket, OpCode, Opaque, Res).
