-module(ranch_memcached_protocol).
-export([start_link/4, init/4]).

-include("rmp_constants.hrl").

-record(lengths, {
    extra,
    key,
    body,
    total
    }).

-record(message, {
    extra,
    key,
    body,
    cas,
    opaque
    }).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(ListenerPid),
    io:format("Got a connection!~n"),
    loop(header, Socket, Transport, Opts),
    ok.

read(Length, Socket, Transport) ->
    case Transport:recv(Socket, Length, 100) of
        {ok, Data} ->
            L = size(Data),
            case L of
                Length -> {ok, Data};
                    _ -> read(Length-L, Socket, Transport, Data)
            end;
        Error -> Error
    end.

read(Length, Socket, Transport, Remains) ->
    case Transport:recv(Socket, Length, 100) of
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
            {ok, Sizes} = handle_header(Data),
            io:format("Sizes: ~p~n", [Sizes]),
            loop(Sizes, Socket, Transport, Opts);
        {error, Error} ->
            io:format("Socket error : ~p~n", [Error])
    end;
loop(#lengths{total=Len}=Sizes, Socket, Transport, Opts) ->
    {ok, Data} = read(Len, Socket, Transport),
    handle_body(Data, Sizes),
    loop(header, Socket, Transport, Opts).

handle_header(<<?REQ_MAGIC:8, Opcode:8, KeyLen:16,
    ExtraLen:8, 0:8, 0:16,
    BodyLen:32,
    Opaque:32,
    CAS:64>>) ->
    {ok, #lengths{extra=ExtraLen, key=KeyLen, body=BodyLen - (KeyLen + ExtraLen), total=BodyLen}}.

handle_body(Data, #lengths{extra=ExtraLen, key=KeyLen, body=BodyLen}=Sizes) ->
    io:format("Body, ~p ~p ~p ~p ~n", [size(Data), Sizes, Data, {ExtraLen, KeyLen, BodyLen} ]),
    EL = ExtraLen * 8,
    KL = KeyLen * 8,
    BL = BodyLen * 8,
    <<Extra:EL/bitstring, Key:KL/bitstring, Body:BL/bitstring>> = Data,
    Message = #message{extra=Extra, key=Key, body=Body},
    io:format("Message ~p~n", [Message]),
    ok.

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
