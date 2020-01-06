# Digamma

R7RS/R6RS Scheme Implementation derived from Ypsilon ([fujita-y/ypsilon](https://github.com/fujita-y/ypsilon)).

Digamma implements mostly concurrent garbage collector that achieves a remarkably short GC pause time and the best performance in parallel execution.
Digamma virtual machine supports native thread to execute arbitrary scheme code in parallel.
Each execution thread have own GC thread run independently from others.
For example, when applying parallel map to 4 elements with using maximum parallelism, digamma starts new 8 native thread run simultaneously (program x 4 + GC x 4).

See [LICENSE](https://github.com/fujita-y/digamma/blob/master/LICENSE) file for terms and conditions of use.

### Special note for MacOS users

You may want try "export MACOSX_DEPLOYMENT_TARGET=10.14" while build if you got "Segmentation fault: 11 (stack_not_16_byte_aligned_error)" on execution. https://forums.developer.apple.com/thread/121887

### Run

* To run R7RS script from project root, try following:
```
./digamma --r7rs --top-level-program --disable-acc --sitelib=./stdlib -- test/r7rs-sample.scm
```

### Limitations and Notes

* REPL start with '(import (core))' regardless what command line option is given.
* Without '-top-level-program', the contents of the specified script file will be interpreted as if they had been entered into the REPL.

### Rebuild Heap Files

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
