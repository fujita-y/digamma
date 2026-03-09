```
$ cd github/digamma
$ gosh boot/build-core-ir.scm
$ cd github/digamma/build
$ ./nanos
> !../boot/core.ir
```

$ ./nanos
> !../boot/core.ir
> ((const r1 (let ((n 1)) (+ n 1))) (global-ref r2 compile) (mov r0 r1) (call r2 1) (ret))
> ((const r1 9) (global-ref r2 macroexpand) (mov r0 r1) (call r2 1) (ret))
> ((const r1 (- 3)) (global-ref r2 macroexpand) (mov r0 r1) (call r2 1) (ret))


((global-ref r0 collect) (mov r1 r0) (call r1 0) (ret))
((global-ref r0 safepoint) (mov r1 r0) (call r1 0) (ret))

gosh boot/build-core-ir.scm
!../boot/core.ir
((const r1 (- 3)) (global-ref r2 macroexpand) (mov r0 r1) (call r2 1) (ret))

