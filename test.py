import bmemcached
client = bmemcached.Client(('127.0.0.1:11211'))

client.set('foo', 'bar')
