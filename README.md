Libuv + Gauche = Violet
=======================

Overview
--------

Violet is a web application server inspired by Node.js. This is based on Libuv and applications can be written in Scheme with the Gauche Scheme interpreter. All the necessary network packet handling is done by Libuv and [Gauche-makiki](https://github.com/shirok/Gauche-makiki).

With the power of Scheme's continuation, you can easily write asynchronous procedures for high-performance web services.

```
Libuv <-> makiki <-> File I/O
                 <-> Network I/O
                 <-> Application implementation
```

Build
---

- Edit config.mk
- `make`
- `sudo make install`

Also, take a look at docker-compose.yml and Dockerfile in the `eg` directory.

Requirements
------------

* [Gauche 0.9.10](https://practical-scheme.net/gauche/gmemo/?Release%200.9.10) or above.
* [libuv | Cross-platform asynchronous I/O](https://libuv.org/)

Depends on
----------

* [makiki](https://github.com/shirok/Gauche-makiki) -- a simple multithreaded http server
* [Rheingau](https://github.com/torus/gauche-rheingau) -- a dumb package manager for Gauche

References
----------
* [An Introduction to libuv](https://nikhilm.github.io/uvbook/index.html)
