Memcached protocol for Ranch
============================

http://dustin.github.com/2009/10/11/ememcached.html

http://cloud.github.com/downloads/memcached/memcached/protocol-binary.txt

http://www.slideshare.net/tmaesaka/memcached-binary-protocol-in-a-nutshell-presentation#btnNext

Test it
-------

    erl -pa deps/*/ebin -pa ebin -boot start_sasl -eval "application:start(ranch)."

    ranch:start_listener(memcached, 10, ranch_tcp, [{port, 11211}], ranch_memcached_protocol_plain, [rmp_dummy_plain, []]).

Licence
-------

MIT ©Mathieu Lecarme 2012
