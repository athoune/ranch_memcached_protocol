-module(gen_memcached).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get, 2}, {set, 5}];

behaviour_info(_) ->
    undefined.

