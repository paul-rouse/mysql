## 0.1.4

* Expose mysql_thread_end() as `endThread`

## 0.1.3

* Safer concurrency - see https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell
* Better support for building against MariaDB (not well tested).
* Additional C binding: mysql_get_server_version().

## 0.1.2.1

* Fix bystestring-valued connectOptions sometimes not being null terminated at the correct place (avoid unsafeUseAsCString).

## 0.1.2

* Fix setup for cabal 1.24
* New maintainer and GitHub location - with many thanks to Bryan O'Sullivan for all of the past work on this module, and for facilitating the transfer of maintenance responsibility.
