Memcached protocol for Ranch
============================

http://dustin.github.com/2009/10/11/ememcached.html

http://cloud.github.com/downloads/memcached/memcached/protocol-binary.txt

Test it
-------

    erl -pa deps/*/ebin -pa ebin -boot startt_sasl -eval "application:start(ranch)."

    ranch:start_listener(memcached, 10, ranch_tcp, [{port, 11211}], ranch_memcached_protocol, []).

Licence
-------

MIT ©Mathieu Lecarme 2012
