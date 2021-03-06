Seax Scheme
===========

[![Join the chat at https://gitter.im/hawkw/seax](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hawkw/seax?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build Status](https://img.shields.io/travis/hawkw/seax_scheme/master.svg?style=flat-square)](https://travis-ci.org/hawkw/seax_scheme)
[![Coverage](https://img.shields.io/codecov/c/github/hawkw/seax_scheme/master.svg?style=flat-square)](http://codecov.io/github/hawkw/seax_scheme?branch=master)
[![Latest RustDoc](https://img.shields.io/badge/rustdoc-latest-green.svg?style=flat-square)](http://hawkweisman.me/seax/api/seax_scheme/)
[![Latest Seax Scheme release](https://img.shields.io/crates/v/seax_scheme.svg?style=flat-square)](https://crates.io/crates/seax_scheme)
[![License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat-square)](https://github.com/hawkw/seax/LICENSE)

A library for compiling Scheme programs for [Seax](http://hawkweisman.me/seax/), a runtime environment for functional programming languages.

This implementation aims to conform with the [Revised<sup>6</sup> Report on Scheme](http://www.r6rs.org) (R<sup>6</sup>RS) whenever possible, but may not be a complatible implementation. `seax-scheme` is released as a library, rather than an executable, so that it may be included in other applications which use Scheme as an embedded language.

Note that since this repository is a library and does not contain the Seax VM, the code in this repository is not sufficient to compile Scheme programs for Seax on its own. The [hawkw/seax](https://github.com/hawkw/seax) repository contains the Seax compiler/interpreter command-line application and REPL, which you will probably want if you intend to develop programs targeting Seax.

Please report all issues and feature requests to the main repository ([hawkw/seax](https://github.com/hawkw/seax)).

Contributing
------------

Seax is an open-source project and contributions are happily welcomed. For more information on how to contribute to Seax, please see the [CONTRIBUTING](https://github.com/hawkw/seax/blob/master/CONTRIBUTING.md) document on the main Seax repository.
