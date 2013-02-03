-module(gen_memcached).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1}, {get, 2}, {set, 5}, {delete, 2}, {stats, 1}];

behaviour_info(_) ->
    undefined.

