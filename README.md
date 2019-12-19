# digamma

Note: How to rebuild heap on changing identifiers

* Go src/core.h and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* 'make'
* 'cd heap'
* Edit/delete identifiers in library.scm
* 'make'
* 'cd ..'
* 'make'
* 'cd heap'
* 'make'
* 'cd ..'
* (Changes should be available in binary at this point)
* Revert 'UNBOUND_GLOC_RETURN_UNSPEC' back to '0'
* make
