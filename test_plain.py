#!/usr/bin/env python

import memcache

mc = memcache.Client(['127.0.0.1:11211'], debug=0)

print mc.set("some_key", "Some value")
print mc.set("beuha", "Aussi")
#value = mc.get("some_key")
