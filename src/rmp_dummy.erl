-module(rmp_dummy).

-export([handle/3]).

-include("rmp_constants.hrl").

handle(Conn, ?SET, #rmp_message{}=Message) ->
    io:format("Handle ~p~n", [Message]),
    ranch_memcached_protocol:respond(Conn, ?SUCCESS, 0, #rmp_response{});
handle(_Conn, Opcode, Message) ->
    io:format("Oups handle: ~p ~p ~n", [Opcode, Message]).
