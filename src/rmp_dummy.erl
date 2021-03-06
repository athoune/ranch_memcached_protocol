-module(rmp_dummy).

-export([handle/3]).

-include("rmp_constants.hrl").

handle(Conn, ?SET, #rmp_message{}=Message) ->
    io:format("Handle SET ~p~n", [Message]),
    ranch_memcached_protocol:respond(Conn, ?SUCCESS, #rmp_response{});
handle(Conn, ?GETKQ, #rmp_message{}=Message) ->
    handle(Conn, ?GET, Message);
handle(Conn, ?GET, #rmp_message{}=Message) ->
    io:format("Handle GET ~p~n", [Message]),
    ranch_memcached_protocol:respond(Conn, ?SUCCESS, #rmp_response{value="Aussi"});
handle(Conn, ?VERSION, #rmp_message{}=_) ->
    ranch_memcached_protocol:respond(Conn, ?VERSION, #rmp_response{value="1.4.2", extra=undefined});
handle(_Conn, Opcode, Message) ->
    io:format("Oups handle: ~p ~p ~n", [Opcode, Message]).
