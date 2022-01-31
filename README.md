# Digamma

R7RS/R6RS Scheme Implementation derived from Ypsilon (fujita-y/ypsilon) for experimental research

* Concurrent garbage collector that achieves a remarkably short GC pause time
* Separate compilation thread to incrementally generate native code in background
* On the fly FFI with native stub code generation

Digamma is an experimental work and may change its design significantly. Research results will be applied to Ypsilon (fujita-y/ypsilon) in the future.

See [LICENSE](https://github.com/fujita-y/digamma/blob/master/LICENSE) file for terms and conditions of use.

### Requirements

LLVM 10

### Build and Install

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
cmake --install .
```

### Run

* To run R7RS sample script, try following from project root:
```
digamma --r7rs --top-level-program --disable-acc -- test/r7rs-sample.scm
```

* To run FFI demo program, try follwing from project root:
```
digamma --r6rs --top-level-program demo/glut-demo.scm # (OpenGL 1.x, GLUT)
digamma --r6rs --top-level-program demo/glfw-demo.scm # (OpenGL 1.x, GLFW)
digamma --r6rs --top-level-program demo/glcorearb-demo.scm # (OpenGL Core Profile, GLFW)
digamma --r6rs --top-level-program demo/freetype-demo.scm # (OpenGL Core Profile, GLFW, freetype)
digamma --r6rs --top-level-program demo/widget-demo.scm # (OpenGL Core Profile, GLFW, freetype)
```

### Limitations and Notes

* REPL start with '(import (core))' regardless what command line option is given.
* Without '--top-level-program', the contents of the specified script file will be interpreted as if they had been entered into the REPL.

### Rebuild Heap Files

* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* ```make```
* Edit .scm files in 'heap/boot' directory
* ```cd heap; make; cd ..; make; cd heap; make; cd ..```
* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* ```make```
