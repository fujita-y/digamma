# Digamma

R7RS/R6RS Scheme Implementation

* Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
* See LICENSE file for terms and conditions of use.

## How to rebuild heap on changing identifiers

* open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* $ make
* $ cd heap
* open 'boot/libraries.scm' and edit
* $ make
* $ cd ..
* $ make
* $ cd heap
* $ make
* $ cd ..
* open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* $ make
