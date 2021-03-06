# Logging Level Types

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

<!--
[![Build](https://travis-ci.org/trskop/lumberjack.svg)](https://travis-ci.org/trskop/lumberjack)
-->


# Description

Library describes two commonly used logging level schemas:

* Syslog style logging levels: `Emergency, Alert, Critical, Error, Warning,
  Notice, Info, and Debug`
* Logging levels common in many production systems, very similar to what e.g.
  log4j uses: `Error, Warning, Info, Debug, and Trace`

Each serious logging framework has to deal with logign levels, but most of them
use their own data types to define them. As a consequence it is harder then it
should be to combine functionality of multiple logging frameworks.


License
-------

The BSD 3-Clause License, see [LICENSE][] file for details.


Contributions
-------------

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail (see `.cabal` file for
that).



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[LICENSE]:
  https://github.com/trskop/lumberjack/blob/master/log-level/LICENSE
  "License of log-level package."
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
