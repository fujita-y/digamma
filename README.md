# Digamma

Digamma is a RnRS scheme system derived from fujita-y/ypsilon.

Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
See LICENSE file for terms and conditions of use.

## How to rebuild heap on changing identifiers

* Open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* $ make
* $ cd heap
* Open 'boot/library.scm' and edit
* $ make
* $ cd ..
* $ make
* $ cd heap
* $ make
* $ cd ..
* Open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* $ make
