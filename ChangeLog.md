## 0.2.0.1

* Make connection finalizer thread safe (#28).

## 0.2

* Remove obsolete `fieldDefault` from `data Field` (#41).

## 0.1.7.3

* Fix error on certain systems introduced by the change in (#40): some implementations of `mysql_config` do not recognise `--libs-sys`, yet return a zero status for attempts to use it.

## 0.1.7.2

* Update .cabal file

## 0.1.7.1

* Use `mysql_config --libs-sys` in Setup.hs if available (#40)

## 0.1.7

* Add support for JSON type

## 0.1.6

* Fix build for mysql 8

## 0.1.5

* Add Semigroup instance for FieldFlags
* Fix some warnings
* Drop testing under GHC 7.8 / lts-2

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
