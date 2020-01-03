# Digamma

R7RS/R6RS Scheme Implementation derived from Ypsilon ([fujita-y/ypsilon](https://github.com/fujita-y/ypsilon)).

* Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
* See LICENSE file for terms and conditions of use.

### Run

* To run R7RS script from project root, try following:
```
./digamma --r7rs --top-level-program --disable-acc --sitelib=./sitelib:./stdlib -- test/r7rs-sample.scm
```

### Limitations

* REPL start with '(import (core))' regardless what command line option is given.

### Rebuild heap files

* open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* $ make
* $ cd heap
* open and edit 'boot/libraries.scm' for example
* $ make
* $ cd ..
* $ make
* $ cd heap
* $ make
* $ cd ..
* open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* $ make
