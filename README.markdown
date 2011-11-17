# mysql: bindings to the mysqlclient library

This library is a Haskell binding to the MySQL `mysqlclient` client
library.  It is a fairly faithful, low level library that implements
most of the MySQL client API.  The major departure from the C API is
that in Haskell, resource management is mostly automatic and safe.

This library deliberately avoids the question of providing a "good"
API. Its purpose is to serve as a base upon which higher-level
libraries can be built.

# Licensing

This library is BSD-licensed under the terms of the
[MySQL FOSS License Exception](http://www.mysql.com/about/legal/licensing/foss-exception/).

Since this library links against the GPL-licensed `mysqlclient`
library, a non-open-source application that uses it *may* be subject
to the terms of the GPL.

# To do

* Add support for prepared statements. The prepared statement API is
  huge and of dubious performance value, so it's not currently a
  priority for us. Patches welcome!

# Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/bos/mysql/issues).

Master [git repository](http://github.com/bos/mysql):

* `git clone git://github.com/bos/mysql.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/mysql):

* `hg clone http://bitbucket.org/bos/mysql`

(You can create and contribute changes using either git or Mercurial.)

# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
