# hexpm.mk

A plugins for [erlang.mk](http://erlang.mk) to use hex dependencies.

This plugins allow you to specify hex dependencies operators. So it supports `>=`, `<=`, `>`, `<`, `==` and `~>`.

> Our plugin [to generate mix file](https://github.com/botsunit/mix.mk) is compatible with this one.

## Example

```makefile
PROJECT = test

DEP_PLUGINS = hexpm.mk
BUILD_DEPS = hexpm.mk

dep_hexpm.mk = git https://github.com/botsunit/hexpm.mk.git master

DEPS = jsx
dep_jsx = hex ~> 2.6.0

include erlang.mk
```

> If the package index does not exist, hexpm.mk will create it. But if it exist, it's up to you to update it.

## Targets

*hexpm.mk* add two targets :

* `hex-update` : Updates the package index.
* `hex-search p=...` : Search a package in the package index.

## Licence

Copyright (c) 2016, Bots Unit<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


