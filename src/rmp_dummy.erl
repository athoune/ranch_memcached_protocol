-module(rmp_dummy).

-export([handle/3]).

-include("rmp_constants.hrl").

handle(Conn, ?SET, #rmp_message{}=Message) ->
    io:format("Handle ~p~n", [Message]),
    ranch_memcached_protocol:respond(Conn, ?SUCCESS, #rmp_response{});
handle(Conn, ?GET, #rmp_message{key=Key}=Message) ->
    io:format("Handle ~p~n", [Message]),
    ranch_memcached_protocol:respond(Conn, ?SUCCESS, #rmp_response{key=Key, body=Key});
handle(_Conn, Opcode, Message) ->
    io:format("Oups handle: ~p ~p ~n", [Opcode, Message]).
