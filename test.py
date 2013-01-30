import bmemcached
client = bmemcached.Client(('127.0.0.1:11211'))
print client.set('foo', 'bar')
print client.get('beuha')
print client.get('beuha')
