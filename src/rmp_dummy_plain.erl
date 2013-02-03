-module(rmp_dummy_plain).
-behaviour(gen_memcached).

-export([init/1, get/2, set/5, delete/2]).

-record(dummy, {store}).

init(_Opts) ->
    {ok, #dummy{store=dict:new()}}.

get(Key, #dummy{store=Store}=Opts) ->
    case dict:find(Key, Store) of
        error ->
            {none, Opts};
        {ok, Value} ->
            {ok, Value, Opts}
    end.

set(Key, _Flags, _Exptime, Data, #dummy{store=Store}=Opts) ->
    {ok, Opts#dummy{store=dict:store(Key, Data, Store)}}.

delete(Key, #dummy{store=Store}=Opts) ->
    case dict:find(Key, Store) of
        error ->
            {none, Opts};
        {ok, _} ->
            {ok, Opts#dummy{store=dict:erase(Key, Store)}}
    end.
