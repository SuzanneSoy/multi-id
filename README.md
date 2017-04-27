[![Build Status,](https://img.shields.io/travis/jsmaniac/multi-id/master.svg)](https://travis-ci.org/jsmaniac/multi-id)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/multi-id/master.svg)](https://codecov.io/gh/jsmaniac/multi-id)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/multi-id)
[![Online Documentation,](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/multi-id/)
[![Maintained as of 2017,](https://img.shields.io/maintenance/yes/2017.svg)](https://github.com/jsmaniac/multi-id/issues)
[![License: CC0 v1.0.](https://img.shields.io/badge/license-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)


multi-id
========

This package helps defining identifiers with many different meanings in
different contexts. An identifier can be given a meaning:

* As a [type expander](http://github.com/jsmaniac/type-expander) `(: foo (Listof (ident arg …)))`
* As a match expander
* As a called function
* As a simple identifier (i.e. used as a variable)
* As a `set!` subform

Installation
------------

Install with:

```
raco pkg install --deps search-auto multi-id
```
