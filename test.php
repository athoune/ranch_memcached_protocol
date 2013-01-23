<?php
// pecl install memcached
$m = new Memcached();
$m->setOption(Memcached::OPT_BINARY_PROTOCOL, True);
$m->setOption(Memcached::OPT_COMPRESSION, False);
$m->addServer('localhost', 11211);

var_dump($m->set('foo', 100));
var_dump($m->getResultCode());
var_dump($m->get('foo'));
var_dump($m->getResultCode());
// http://php.net/manual/fr/memcached.getresultcode.php
?>
