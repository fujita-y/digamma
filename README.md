# Digamma

R7RS/R6RS Scheme Implementation derived from Ypsilon ([fujita-y/ypsilon](https://github.com/fujita-y/ypsilon)).

Digamma implements mostly concurrent garbage collector that achieves a remarkably short GC pause time, implements separate compilation thread to incrementally generate native code in background, implements on the fly FFI with LLVM.

Digamma is an experimental work and may change its design significantly.

See [LICENSE](https://github.com/fujita-y/digamma/blob/master/LICENSE) file for terms and conditions of use.

### Requirements

LLVM 10

### Run

* To run R7RS script from project root, try following:
```
./digamma --r7rs --top-level-program --disable-acc -- test/r7rs-sample.scm
```

* To run FFI demo program (OpenGL, GLUT):
```
./digamma --r6rs --top-level-program demo/glut-demo.scm
```

### Limitations and Notes

* REPL start with '(import (core))' regardless what command line option is given.
* Without '--top-level-program', the contents of the specified script file will be interpreted as if they had been entered into the REPL.

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
