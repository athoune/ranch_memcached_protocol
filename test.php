<?php
// pecl install memcached
$m = new Memcached();
$m->setOption(Memcached::OPT_BINARY_PROTOCOL, True);
$m->addServer('localhost', 11211);

$m->set('foo', 100);
var_dump($m->get('foo'));
?>
